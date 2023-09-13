package net.thecoda.desynced.codec

import jdk.internal.org.jline.utils.WCWidth
import scodec.bits.BitVector
import scodec.codecs.{provide, uint8}
import scodec.{Attempt, Codec, DecodeResult, Decoder, SizeBound}
import scodec.Attempt.{Failure, Successful}

import scala.annotation.tailrec

object PackedIntCodec extends Codec[Long] {
  @tailrec
  def trimBitVec(bv: BitVector): BitVector =
    bv.headOption match {
      case Some(false) => trimBitVec(bv.tail)
      case _ => bv
    }

  def rightGroupBitVec(bv: BitVector, width: Int): Seq[BitVector] = {
    @tailrec
    def recurse(bv: BitVector, acc: List[BitVector] = Nil): List[BitVector] =
      if bv.size > width
      then recurse(bv.dropRight(width), bv.takeRight(width) :: acc)
      else bv :: acc

    recurse(bv)
  }

  def decodePackedIntBits: Decoder[BitVector] =
    uint8.flatMap { byte =>
      val shiftedByte = byte >> 1
      val packedBits = BitVector.fromInt(shiftedByte, 7)
      if ((byte & 0x1) == 0x1) {
        decodePackedIntBits.map(pb2 => packedBits ++ pb2)
      } else {
        provide(packedBits)
      }
    }

  override def decode(bits: BitVector): Attempt[DecodeResult[Long]] =
    decodePackedIntBits.map(bv => bv.toLong(signed = false)).decode(bits)

  override def encode(value: Long): Attempt[BitVector] = {
    val bv = trimBitVec(BitVector.fromLong(value))
    val groups = rightGroupBitVec(bv, 7)
    Successful(
      BitVector.concat(
        groups.init.map(_ :+ true) :+ (groups.last :+ false)
      )
    )
  }

  override def sizeBound: SizeBound =
    SizeBound(lowerBound = 8, upperBound = None)
}
