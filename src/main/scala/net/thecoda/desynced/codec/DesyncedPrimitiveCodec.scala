package net.thecoda.desynced.codec

import scodec.Codec
import scodec.Attempt.{Failure, Successful}
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err, SizeBound}
import scodec.bits.BitVector
import scodec.bits.*
import scodec.codecs.*
import scodec.codecs.given
import scodec.Codec.given

import scala.annotation.tailrec


object DesyncedPrimitiveCodec extends Codec[DPrimitive] {

  val dnil: Codec[DNil.type] = constant(hex"c0") ~> provide(DNil)

  val dFalse: Codec[DFalse.type] = constant(hex"c2") ~> provide(DFalse)
  val dTrue: Codec[DTrue.type] = constant(hex"c3") ~> provide(DTrue)

  val dInvalid: Codec[DInvalid.type] = constant(hex"c4") ~> provide(DInvalid)
  val dDeadKey: Codec[DDeadKey.type] = constant(hex"c5") ~> provide(DDeadKey)

  val dfloat32: Codec[DFloat32] =
    (constant(hex"ca") :: float).dropUnits.as[DFloat32]
  val dfloat64: Codec[DFloat64] =
    (constant(hex"cb") :: double).dropUnits.as[DFloat64]

  val duint8: Codec[DUInt8] =
    (constant(hex"cc") :: uint8).dropUnits.as[DUInt8]
  val duint16: Codec[DUInt16] =
    (constant(hex"cd") :: uint16).dropUnits.as[DUInt16]
  val duint32: Codec[DUInt32] =
    (constant(hex"ce") :: uint32).dropUnits.as[DUInt32]
  val duint64: Codec[DUInt64] =
    (constant(hex"cf") :: long(64)).dropUnits.as[DUInt64]

  val dint8: Codec[DInt8] =
    (constant(hex"d0") :: int8).dropUnits.as[DInt8]
  val dint16: Codec[DInt16] =
    (constant(hex"d1") :: int16).dropUnits.as[DInt16]
  val dint32: Codec[DInt32] =
    (constant(hex"d2") :: int32).dropUnits.as[DInt32]
  val dint64: Codec[DInt64] =
    (constant(hex"d3") :: int64).dropUnits.as[DInt64]

  val dfixStr: Codec[DFixString] =
    (constant(bin"101") :: variableSizeBytes(uint(5), utf8)).dropUnits.as[DFixString]
  val dstr8: Codec[DString8] =
    (constant(hex"d9") :: variableSizeBytes(uint8, utf8)).dropUnits.as[DString8]
  val dstr16: Codec[DString16] =
    (constant(hex"da") :: variableSizeBytes(uint16, utf8)).dropUnits.as[DString16]
  val dstr32: Codec[DString32] =
    (constant(hex"db") :: variableSizeBytesLong(uint32, utf8)).dropUnits.as[DString32]

  val dpositiveFixInt: Codec[DPositiveFixInt] =
    (constant(bin"0") :: uint(7)).dropUnits.as[DPositiveFixInt]

  val dnegativeFixInt: Codec[DNegativeFixInt] =
    (constant(bin"111") :: uint(5).xmap(_ - 0x20, (a: Int) => a + 0x20)).dropUnits.as[DNegativeFixInt]

  private val codec: Codec[DPrimitive] =
    scodec.codecs.lazily {
      choice(
        dpositiveFixInt.upcast,
        dnegativeFixInt.upcast,
        dInvalid.upcast,
        dDeadKey.upcast,
        dnil.upcast,
        dTrue.upcast,
        dFalse.upcast,
        dfloat32.upcast,
        dfloat64.upcast,
        duint8.upcast,
        duint16.upcast,
        duint32.upcast,
        duint64.upcast,
        dint8.upcast,
        dint16.upcast,
        dint32.upcast,
        dint64.upcast,
        dfixStr.upcast,
        dstr8.upcast,
        dstr16.upcast,
        dstr32.upcast,
      )
    }

  export codec.{encode, decode, sizeBound}
}
