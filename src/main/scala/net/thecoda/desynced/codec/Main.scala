package net.thecoda.desynced.codec

import scodec.Attempt
import scodec.Attempt.{Failure, Successful}

import java.util.zip.Inflater
import java.nio.ByteBuffer

def byteToBin(byte: Byte): String =
  (byte & 0xFF).toBinaryString.reverse.padTo(8, '0').reverse

def byteToHex(byte: Byte): String =
  "0x" + (byte & 0xFF).toHexString.reverse.padTo(2, '0').reverse

def byteToDec(byte: Byte): String =
  (byte & 0xFF).toString.reverse.padTo(3, '0').reverse

def byteToPrintChar(byte: Byte): String =
  val char = byte.toChar
  if char >= '!' && char <= '~' then char.toString else " "

def process(input: String): Unit = {
  val cleanedInput = CleanedInput(input)
  val b62buf = cleanedInput.toBase62Buffer
//  println(s"b62 = ${b62buf.underlying}")

  val (decompressLen, remainder) = b62buf.readSingleInt
  assert(decompressLen <= 20 * 1024 * 1024, "Input data is over 20MB")

  println(s"data type was ${cleanedInput.dataType}")
  println(s"compressed length was $decompressLen")

  val decompressed = b62buf.decodeToByteVector
  println("decompressed: " + decompressed.toHex.grouped(2).mkString(" "))
  println("decompressed: " + decompressed.toArray.zipWithIndex.map((b, idx) => "%03d".formatted(idx) + " " + byteToPrintChar(b) + " " + byteToDec(b) + " " + byteToHex(b) + " " + byteToBin(b)).mkString("\n"))

  DesyncedDataCodec.complete.decode(decompressed.toBitVector) match {
    case Successful(x) =>
      println("result: ")
      pprint.pprintln(x.value)
      println("remainder: " + x.remainder.toHex.grouped(2).mkString(" "))
    case Failure(x) =>
      DesyncedTableCodec.log.dumpToConsole()
      println("failure: " + x.messageWithContext)
  }
}

@main def main(): Unit = {
  process("DSC8i2aZu6y0tMLNB0PBhLM2MZw2U2EWgeL4CeghO42QyxI30UveL3DsdAx2UQyIb3vboyg2yF0dP4gajEf17c76B2cMtNZ1G57xk0FLz5s4BRVnw0VVs3G12nxvw1eQUv60Vg9an3KWUKb3JJHBQ3iyF290xR1mT1RrEJp2pekYH30fFut1ToSvC1ek5Gl0bA9OP2UiVaA4UzsKP0yT4oR31Glje0wYKFh2Cajqe0qc3ER0PIc5Y0bWajY0K6MzE3UOlFU0wYPRf0lG4Sa29vMUB0tHOcY08dZj43ZQUlU49Zv")
  //  process("DSCV2KK")
  //  process("DSCV3HEnIW1WNkpJ1verE81D517Z2APS5d30iKK11rA1Ms00q")
}
