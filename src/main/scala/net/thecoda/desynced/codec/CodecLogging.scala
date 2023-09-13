package net.thecoda.desynced.codec

import scodec.*
import scodec.Attempt.{Failure, Successful}
import scodec.codecs.*
import scodec.codecs.given
import scodec.Codec.given
import scodec.bits.*


class CodecLogging(log: TreeLogger) {
  extension (name: String)
    private def loggedDecode[A, R](input: A, fn: A => Attempt[DecodeResult[R]]): Attempt[DecodeResult[R]] = {
      log.start(name, s"[for $input]")
      val result = log.withIndent(fn(input))
      val closeText: String = result match {
        case Successful(x) => s"${x.value} [${x.remainder.size} remaining]"
        case Failure(err) => s"${err.messageWithContext}"
      }
      log.end(name, closeText)
      result
    }

    inline def |>[A](codec: Codec[A]): Codec[A] = name | new Codec[A]:
      override def sizeBound: SizeBound = codec.sizeBound

      override def encode(value: A): Attempt[BitVector] = codec.encode(value)

      override def decode(bits: BitVector): Attempt[DecodeResult[A]] =
        loggedDecode(bits, codec.decode)
}
