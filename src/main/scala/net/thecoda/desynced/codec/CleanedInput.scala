package net.thecoda.desynced.codec

import scala.util.Either

case class CleanedInput(rawInput: String) {

  def convertChar(base62: Char) : Either[String,Int]= base62 match {
    case x if x >= '0' && x <= '9' => Right(x.intValue - '0')
    case x if x >= 'A' && x <= 'Z' =>Right(x.intValue - 'A' + 10)
    case x if x >= 'a' && x <= 'z' =>Right(x.intValue - 'a' + 36)
    case x => Left(s"Unable to parse '$x''")
  }

  val sanitised: String =
    rawInput
      .filterNot(_.isWhitespace)
      .stripPrefix("\"")
      .stripSuffix("\"")

  val dataType: Char = sanitised(2)

  val contentAsInts: List[Int]=
    sanitised
      .drop(3)
      .map(convertChar)
      .collect {
        case Right(x)=>x
        case Left(err)=>throw new RuntimeException(err)
      }
      .toList

  def toBase62Buffer: Base62Buffer = Base62Buffer(contentAsInts)

//  println(s"sanitisedInput = ${sanitised.map(_.intValue)}")
  assert(sanitised.length>=5,"Input string must be at least 5 characters long")
  assert(sanitised.startsWith("DS"),"Input string must start with the characters 'DS'")
}