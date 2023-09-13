package net.thecoda.desynced.codec

import scodec.Attempt.{Failure, Successful}
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err, SizeBound}
import scodec.bits.BitVector
import scodec.bits.*
import scodec.codecs.*
import scodec.codecs.given
import scodec.Codec.given

import scala.annotation.tailrec

object DesyncedTableCodec extends Codec[DesyncedData] {
  val log = new TreeLogger
  val codecLogging = CodecLogging(log)
  import codecLogging.{given, *}

  def intPacked: Codec[Long] = "intPacked" |> PackedIntCodec

  //fast, thanks to intrinsics
  private def log2(bits: Int) : Int =
    if (bits <= 0) 0 else 31 - Integer.numberOfLeadingZeros(bits)

  case class MapRow(value: DesyncedData, key: DPrimitive, isArr: Boolean)

  private val keyedMapRow: Codec[MapRow] =
    "mapRow" |> (DesyncedDataCodec :: DesyncedPrimitiveCodec :: intPacked).xmap(
      (v, k, _) => MapRow(v, k, isArr = false),
      entry => (entry.value, entry.key, 0) //TODO: calculate what this packed int should be
    )

  def multipleMapRows(arraySize: Long, mapSize: Long) = new Codec[Vector[MapRow]] {

    override def sizeBound: SizeBound = SizeBound(8 * (arraySize + mapSize), None)

    override def encode(value: Vector[MapRow]) = ???

    @tailrec
    def recurseArray(
        bits: BitVector,
        vacancyBits: BitVector,
        arrayRemaining: Long,
        mapRemaining: Long,
        arrayIdx: Long = 0,
        maskBit: Int = 0,
        acc: List[MapRow] = Nil
    ): Either[Err, (Vector[MapRow], BitVector)] = {
      if arrayRemaining == 0 then {
        log.atom(s"array entries done")
        recurseMap(bits, vacancyBits, mapRemaining, maskBit, acc)
      } else {
        log.atom(s"--- array $arrayIdx [$arrayRemaining + $mapRemaining remain], maskBit = $maskBit ---")
        log.atom("bits: " + bits.toHex.grouped(2).mkString(" "))
        if maskBit > 7 then {
          val vac = bits.take(8)
          recurseArray(bits.drop(8), vac, arrayRemaining, mapRemaining, arrayIdx, 0, acc)
        } else if vacancyBits.dropRight(maskBit).last then {
          log.atom(s"VACANT")
          recurseArray(bits, vacancyBits, arrayRemaining - 1, mapRemaining, arrayIdx + 1, maskBit + 1, acc)
        } else {
          DesyncedDataCodec.decode(bits) match {
            case Successful(dr) =>
              log.atom(s"array entry: $dr")
              val newEntry = MapRow(dr.value, DUInt32(arrayIdx), isArr = true)
              recurseArray(
                dr.remainder,
                vacancyBits,
                arrayRemaining - 1,
                mapRemaining,
                arrayIdx + 1,
                maskBit + 1,
                newEntry :: acc
              )
            case Failure(err) => Left(err)
          }
        }
      }
    }

    @tailrec
    def recurseMap(
        bits: BitVector,
        vacancyBits: BitVector,
        mapRemaining: Long,
        maskBit: Int,
        acc: List[MapRow]
    ): Either[Err, (Vector[MapRow], BitVector)] = {
      if mapRemaining == 0 then {
        log.atom(s"map entries done")
        Right(acc.reverse.toVector, bits)
      } else {
        log.atom(s"--- map [$mapRemaining remain], maskBit = $maskBit ---")
        log.atom("bits: " + bits.toHex.grouped(2).mkString(" "))
        if maskBit > 7 then {
          val vac = bits.take(8)
          recurseMap(bits.drop(8), vac, mapRemaining, 0, acc)
        } else if vacancyBits.dropRight(maskBit).last then {
          log.atom(s"VACANT")
          recurseMap(bits, vacancyBits, mapRemaining - 1, maskBit + 1, acc)
        } else {
          keyedMapRow.decode(bits) match {
            case Successful(dr) =>
              log.atom("remainder: " + dr.remainder.toHex.grouped(2).mkString(" "))
              recurseMap(dr.remainder, vacancyBits, mapRemaining - 1, maskBit + 1, dr.value :: acc)
            case Failure(err) => Left(err)
          }
        }
      }
    }

    override def decode(bits: BitVector): Attempt[DecodeResult[Vector[MapRow]]] = {
      recurseArray(bits.drop(8), bits.take(8), arraySize, mapSize) match {
        case Left(err) => Failure(err)
        case Right(vec, remain) => Successful(DecodeResult(vec, remain))
      }
    }
  }

  private def dmapOfSizes(arraySize: Long, mapSize: Long): Codec[DMap] = {
    def entriesToMap(entries: Vector[MapRow]): DMap =
      DMap(
        entries.takeWhile(_.isArr).map(_.value),
        entries.dropWhile(_.isArr).map(entry => (entry.key, entry.value)).toMap,
      )

    def mapToEntries(fixMap: DMap): Vector[MapRow] =
      val arrPart = fixMap.a.zipWithIndex.map { case (v, k) => MapRow(v, DPositiveFixInt(k), isArr = true) }
      val mapPart = fixMap.m.toVector.map { case (k, v) => MapRow(v, k, isArr = false) }
      arrPart ++ mapPart

    s"dtableData data($arraySize, $mapSize)" |>
      multipleMapRows(arraySize, mapSize).xmap(entriesToMap, mapToEntries)
  }

  private def sizeExponent(bitCount: Int): Codec[Int] =
    uint(bitCount).xmap(1 << _, log2)

  private def dmapWithSizeCodec(sizeExponentCodec: Codec[Int]): Codec[DMap] =
    lazily {
      "dmap" |> sizeExponentCodec.flatPrepend { mapSize =>
        ("arrayflag" |> bool).flatPrepend{ hasArray =>
          ("arraysize" |> withDefault(conditional(hasArray, intPacked), provide(0))).flatPrepend {
            arraySize => intPacked :: dmapOfSizes(arraySize, mapSize)
          }
        }
      }.xmap(
        {case (_, _, _, _, dmap) => dmap},
        {case dmap => (dmap.m.size, dmap.a.nonEmpty, dmap.a.size, 0, dmap)}
      )
    }

  val dfixMap: Codec[DMap] =
    (constant(bin"1000") :: dmapWithSizeCodec(sizeExponent(3))).dropUnits.as[DMap]

  //  val dmap16: Codec[DMap16] =
  //    (constant(hex"de") :: dmap(sizeExponent(15))).dropUnits.as[DMap16]
  //
  //  val dmap32: Codec[DMap32] =
  //    (constant(hex"df") :: dmap(sizeExponent(31), bool)).dropUnits.as[DMap32]


  val dmap: Codec[DMap] = dfixMap

//  val dmap: Codec[DMap] = Codec(
//    encoder = (value: DMap) => {
//      if value.a.size <= 128 then dfixMap
//      else if value.a.size <= 65536 then dmap16
//      else dmap32
//    },
//    decoder = choice(dfixMap).decode
//  )



  private def darrayWithSizeCodec(sizeCodec: Codec[Int]): Codec[DArray] =
    lazily {
      sizeCodec.flatPrepend(size => dmapOfSizes(size, 0).tuple ).xmap(
        { case (_, dmap) => DArray(dmap.a) },
        { case arr => (arr.a.size, DMap(a = arr.a, m = Map.empty)) }
      )
    }

  val dfixArray: Codec[DArray] =
    (constant(bin"1001") :: darrayWithSizeCodec(uint4)).dropUnits.as[DArray]

  val darray16: Codec[DArray] =
    (constant(hex"dc") :: darrayWithSizeCodec(uint16)).dropUnits.as[DArray]

//  val darray32: Codec[DArray] =
//    (constant(hex"dd") :: darrayWithSizeCodec(uint32)).dropUnits

  val darray: Codec[DArray] = new Codec[DArray] {
    override def encode(value: DArray): Attempt[BitVector] = {
      if value.a.size <= 15 then dfixArray.encode(value)
      else if value.a.size <= 255 then darray16.encode(value)
      else ??? // darray32
    }

    override def decode(bits: BitVector): Attempt[DecodeResult[DArray]] =
      choice(dfixArray, darray16).decode(bits) // darray32

    override def sizeBound: SizeBound = SizeBound(16L, None)
  }

  private val codec: Codec[DesyncedData] =
    lazily {
      choice(
        dfixMap.upcast,
        dfixArray.upcast,
        darray.upcast,
//        darray32.upcast,
//        dmap16.upcast,
//        dmap32.upcast,
      )
    }

  export codec.{encode, decode, sizeBound}
}