package net.thecoda.desynced.codec

import pprint.{PPrinter, Tree}
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


  lazy val printHandlers: PartialFunction[Any, Tree] = {
    case n: DNumber => pprint.Tree.Literal(n.v.toString)
    case DString(str) => pprint.Tree.Literal("\"" + str + "\"")
    case DArray(vec) => customPprint.treeify(vec.toArray, false, true)
    case dm: DMap => customPprint.treeify(dm.toSimpleMap, false, true)

  }

  lazy val customPprint = pprint.copy(additionalHandlers = printHandlers)

  DesyncedDataCodec.complete.decode(decompressed.toBitVector) match {
    case Successful(x) =>
      println("result: ")
      customPprint.pprintln(x.value)
      println("remainder: " + x.remainder.toHex.grouped(2).mkString(" "))
    case Failure(x) =>
      DesyncedTableCodec.log.dumpToConsole()
      println("failure: " + x.messageWithContext)
  }
}

@main def main(): Unit = {

  process("DSJ1DW2im4Ua1Bde7n2vxqS040XUvn3L25R32MhpPw3FGGkE3rO19P3AcKYe1uSk1c1OZFVZ3rvM0V29BsXB1nVupM1w8ANw1kQNJP2Kc4Qo20woDQ2Ozaay0j8SLZ0fNviq2A9q5b1UHthy13Pvlp3eDaHc2EJ6iX3YrKWh3LQUJh4AYO7M33coo40Y9cMD2ZpyG40S54l42LnjRe4RREZ51U9DrU0PYq7O3tT2MV1McjET2wk8Dj4BEhuK2zjiMy4g7cQq2jupla3u4U5810nhcm1zxOV83NeiOW48Sl8X2yY9wj1zfI482mrD130Z9QjZ2bVcrF4L8z5c2u8uYC2PWE8P1iE5Hw0Uc1Bf10weGg2RJj5d28HbaL4Rz2X41dDmwx4TKA3211lJGM1OZWkN0ZEZii0py1e91bkzMf2Dguzl2uho7I4SjQJh0htMYB1R8sCF0YIq0c1ndvk21MLCNr3qfeDw0jGOGn3HPt8d19muGH4aU7EA3pduvv01d8C61OzQia4FxX200XyMKw3lZjR448wk2X3lZhrv1woqJg3hBUEP41vgwX0o83ck3D3sXJ1fgPlE3kffLK4AdpKS0xfMx30oQ9F42ypCJ92hX9el3FdzUh3yX1Xb3GcLtb4QCVXB11WH2Q1gL4xZ1QfG1q408Wo53FibqO18IAmm2z3r4p2QWU6W1WcMwH3hfx4x3UI9Ii4cXaQr2Wutk33v7")

  //process("DSCIY2jeWAy1njSUc2rlwBu0kDCOW24tOs30Ku73R0LgIzr1Zk64J1Qcp8g0YY7Oo0THH9a4QbCQr3639uj2jUAXZ363AzR2oOqBV2bG6641PvEm21SIxot1L4Mcl1JxXxd1Odj5w0a5wsx1erGxO3u0Bvt47UyGy2V2HpN3cPVLB0CB5qH36hr8I0572Tb4CbR592VrImH1TQUtF0Uz2Fu1rKTkR3cGW8L2Lvu8e1JkqC82EyryO1G4Xj706ypPi2n9RUO0mZbwj3coJHC0fkp2R")
  // TreeMap(
  //  1 -> TreeMap("op" -> "call", "sub" -> 1),
  //  2 -> TreeMap(
  //    1 -> false,
  //    2 -> 4,
  //    3 -> 5,
  //    4 -> 6,
  //    5 -> 7,
  //    6 -> 8,
  //    7 -> 9,
  //    8 -> 10,
  //    9 -> 11,
  //    10 -> 12,
  //    11 -> 13,
  //    "DEAD_KEY" -> NIL,
  //    "op" -> "check_category_item_ii"
  //  ),
  //  3 -> TreeMap("next" -> false, "op" -> "get_filter_package_ii"),
  //  4 -> TreeMap("next" -> false, "op" -> "get_filter_package_ii"),
  //  5 -> TreeMap("next" -> false, "op" -> "get_filter_package_ii"),
  //  6 -> TreeMap("next" -> false, "op" -> "get_filter_package_ii"),
  //  7 -> TreeMap("next" -> false, "op" -> "get_filter_package_ii"),
  //  8 -> TreeMap("next" -> false, "op" -> "get_filter_package_ii"),
  //  9 -> TreeMap("next" -> false, "op" -> "get_filter_package_ii"),
  //  10 -> TreeMap("next" -> false, "op" -> "get_filter_package_ii"),
  //  11 -> TreeMap("next" -> false, "op" -> "get_filter_package_ii"),
  //  12 -> TreeMap("next" -> false, "op" -> "get_filter_package_ii"),
  //  13 -> TreeMap("DEAD_KEY" -> NIL, "op" -> "get_filter_package_ii"),
  //  "name" -> "Custom Name",
  //  "parameters" -> Array(false, false, false, false),
  //  "pnames" -> Array("Param 1", "Param 2", "Param 3", "Param 4"),
  //  "subs" -> Array(TreeMap("name" -> "sub"))
  //)

//  process("DSCEw2iW7ei0PRP5E3an1Gd3di3rw3WtPdG0ptEs112x9ar3FDk1T3V8Mtg3HVtXM18dhUC2JQEBn2ze9dR0Dxj5g1NWi9j26nSOA49Dgra3HtEWR2xAisl4BXZ100EHXFd3eWmUt2nBiXR097U3u0P8ccw030gU900E2qb3In3320aC")

//process("DSC3q2jeWAy3kYPtw3fw6YF2UM9i82JSh7B1jCSMz1s8WS11sMYgc1ndAEQ2yyXsc044R5w2nGUIZ4ESF5c0hse9P0GCNAl3AlG4j3CAUJ61Hfzyc4CC7Eo0DoGIh1Yw9Tj1nd8Ub1wg6oA2NKgwD2bGCSR2sG5oY127S5I")

//  process("DSC5b4BLlHM07H4l83v4nEg0gXE1r0eyXFX1tpMGc3dpjE63bH8ND0iAR0B1tovjR0sGBQE2iXOIi2W8v0z4Rm94L4YESLi2e14ks2DYCXo1lAACP11vFbl49xWQW2OWbJY2KQWhI0alXNi1FinPM1LM8W203ckNW19R8gS43a")
//  process("DSB5y2aXhii0tMLNB0Iniuu1ZwlM20l7afI05Mf9w3yVmzT0DiJza1Sq3O42OBJkG0roJKX3hVpfQ4fWmDn0DPMw10rP0Q83AiUif1PBDPh1L6Db44CKGrh0eVBRG1XsS4K1UtXCm1bAelm1nbU2h44E3oN1oUEJQ1eY3C30G45Fz0WAvy236XpJE1ubUom1gZJ9c2xvE490dH8Cg4Edq2J0aqVof2olf1J3AEBRL0FYddA74o9")
//  process("DSCLW1VlZN21BX0qj1ot1FY3I6iiX1yMNgl08kI2X4UFasL39jkNC2BCOgb31yeRz0bPl6y3KhhkW3cfe0y3ReRb911lliT2J7WP34Dc5gW459sp300WbHE2a6auM0r7AyD1ibQ1C1h3h5d2Q1H0j3Mb8gT0ce9ds0d9ydK2DpCK24So3gY0wPHRm1ha4Ff2Au51k0d7NXU37LOaM3q3sYc3mJ0M92JNVNj1DgEcx0q3bEB0N2GJp1DhnYb3Ns1EY3vyyaX03Fgl20fqU3F1BUPVF0HBTNf20PGXD1e58Ka2jZw1a0De1pR4908xV44TqkC28xAO92MSLIQ1opkub2qT1b64Vzf982DYx1L3TnOxK31XJVS0toNKE030Dn437fxzr0E8lW42UB6SX1MIY9g3pF6fC0Mtp6k1VcbIY4RxqQQ2EE6XA08q5Iq0fhpbd2KLmiS2GTFtl3WcBjR3LIysf1zVE7r1xTers0Clj1v19XrZM2APZH02evjJa4Uz2rX4TDe6I3WT")

//  process("DSC8i2aZu6y0tMLNB0PBhLM2MZw2U2EWgeL4CeghO42QyxI30UveL3DsdAx2UQyIb3vboyg2yF0dP4gajEf17c76B2cMtNZ1G57xk0FLz5s4BRVnw0VVs3G12nxvw1eQUv60Vg9an3KWUKb3JJHBQ3iyF290xR1mT1RrEJp2pekYH30fFut1ToSvC1ek5Gl0bA9OP2UiVaA4UzsKP0yT4oR31Glje0wYKFh2Cajqe0qc3ER0PIc5Y0bWajY0K6MzE3UOlFU0wYPRf0lG4Sa29vMUB0tHOcY08dZj43ZQUlU49Zv")
  // TreeMap(
  //  1 -> TreeMap("op" -> "disconnect"),
  //  2 -> TreeMap(1 -> 1, "op" -> "domove"),
  //  3 -> TreeMap(1 -> 1, 2 -> false, "op" -> "dopickup"),
  //  4 -> TreeMap(1 -> "A", 2 -> 1, "op" -> "get_inventory_item"),
  //  5 -> TreeMap("2" -> "A", "next" -> false, "op" -> "checkfreespace"),
  //  6 -> TreeMap(1 -> 2, "op" -> "domove"),
  //  7 -> TreeMap(1 -> 2, 2 -> false, "op" -> "dodrop"),
  //  8 -> TreeMap(1 -> "B", "next" -> 6, "op" -> "get_inventory_item"),
  //  "name" -> "Transport only full/empty",
  //  "parameters" -> Array(false, false),
  //  "pnames" -> Array("Mine", "Storage")
  //)

  // Prog start
  //    -> disconnect
  //       -> domove(p1)
  //          -> dopickup(p1)
  //             -> get_inventory_item(A)  no items --^ disconnect
  //                -> checkfreespace(A) -> can't fit
  //                   -> domove(p2)
  //                      -> dodrop(p2)
  //                         -> get_inventory_item(B) --^ domove

  //  process("DSCV2KK")
  //  process("DSCV3HEnIW1WNkpJ1verE81D517Z2APS5d30iKK11rA1Ms00q")
}
