package net.thecoda.desynced.codec

import scodec.Attempt.{Failure, Successful}
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err, SizeBound}
import scodec.bits.BitVector
import scodec.bits.*
import scodec.codecs.*
import scodec.codecs.given
import scodec.Codec.given

import scala.annotation.tailrec

object DesyncedDataCodec extends Codec[DesyncedData] {
  val dUserData: Codec[DUserData] = constant(hex"c1") ~> provide(DUserData(ByteVector.empty))

  private val codec: Codec[DesyncedData] =
    lazily {
      choice(
        dUserData.upcast,
        DesyncedPrimitiveCodec.upcast,
        DesyncedTableCodec.upcast,
      )
    }

  export codec.{encode, decode, sizeBound}
}
