package net.thecoda.desynced.codec

import scodec.bits.ByteVector

import java.nio.ByteBuffer
import java.util.zip.Inflater
import scala.annotation.tailrec

case class Base62Buffer(underlying: List[Int]) {
  def readSingleInt: (Int, Base62Buffer) = {
    @tailrec
    def recurse(b: List[Int], current: Int = 0): (Int, Base62Buffer) = b match {
      case h :: t =>
        val newCurrent = current * 31 + h % 31
        if (h < 31) {
          recurse(t, newCurrent)
        } else {
          (newCurrent, Base62Buffer(t))
        }
      case Nil =>
        (current, Base62Buffer(Nil))
    }

    recurse(underlying)
  }
  
  private def longToBytes(n: Int, from: Long): Seq[Byte] = {
    @tailrec def recurse(count: Int, remaining: Long, acc: List[Byte]): Seq[Byte] = count match {
      case 0 => acc.reverse
      case n => recurse(n - 1, remaining >> 8, (remaining & 0xFF).byteValue :: acc)
    }

    recurse(n, from, Nil)
  }
  
  def decodeChunk(chunk: Seq[Int]): ByteVector = {
    assert(chunk.length <= 6)
    val bits: Long = chunk.foldLeft(0L)((acc, i) => acc * 62 + i)
//    println(s"bits = $bits")
    val shiftedBits = chunk.length match {
      case 6 => longToBytes(4, bits)
      case 5 => longToBytes(3, bits)
      case 3 => longToBytes(2, bits)
      case 2 => longToBytes(1, bits)
    }
    val ret = ByteVector(shiftedBits)
//    println(s"chunk length ${chunk.length} bytes out = ${ret.toArray.toList.map(_ & 0xFF)}")
    ret
  }

  private def naiveToByteVector: ByteVector = {
    val checksum = underlying.takeRight(1)
    val chunkVectors = underlying.dropRight(1).grouped(6).map(decodeChunk)
    ByteVector.concat(chunkVectors)
  }

  def decodeToByteVector: ByteVector = {
    val (decompressLen, remainder) = readSingleInt

//    println(s"Inflating buffer of len ${underlying.length}")
//    println(s"= ${underlying.length * 6} bits")
//    println(s"= ${underlying.length * 6d / 4d} bytes")
    val expectedLen = underlying.length * 4 / 6

    val remainderVect = remainder.naiveToByteVector

    if (decompressLen == 0) {
      remainderVect
    } else {
      val inflater = new Inflater()
//      println(s"bufToInflate len = ${remainderVect.toSeq.length}")
//      println(s"bufToInflate = ${remainderVect.toSeq.map(_ & 0xff)}")
      inflater.setInput(remainderVect.toByteBuffer)
      val deflateBuffer = ByteBuffer.allocate(decompressLen)
      inflater.inflate(deflateBuffer)
      inflater.end()
      ByteVector.apply(deflateBuffer.array())
    }
  }
}