package net.thecoda.desynced.codec

import net.thecoda.desynced.codec.DesyncedTableCodec.dfixArray
import scodec.bits.*

import scala.collection.SortedMap

// Can't use enum here because it's a nested structure
sealed abstract class DesyncedData

//primitives
sealed abstract class DPrimitive extends DesyncedData

sealed abstract class DNumber extends DPrimitive {
  def v: Int | Long | Float | Double
  override def toString: String = v.toString
}
case class DPositiveFixInt(v: Int) extends DNumber
case class DNegativeFixInt(v: Int) extends DNumber

case class DFloat32(v: Float) extends DNumber
case class DFloat64(v: Double) extends DNumber

case class DUInt8(v: Int) extends DNumber
case class DUInt16(v: Int) extends DNumber
case class DUInt32(v: Long) extends DNumber
case class DUInt64(v: Long) extends DNumber

case class DInt8(v: Int) extends DNumber
case class DInt16(v: Int) extends DNumber
case class DInt32(v: Int) extends DNumber
case class DInt64(v: Long) extends DNumber

case class DString(s: String) extends DPrimitive {
  override def toString: String = s
}

sealed abstract class DBool(val value: Boolean) extends DPrimitive {
  override def toString: String = value.toString
}
case object DFalse extends DBool(false)
case object DTrue extends DBool(true)

case object DNil extends DPrimitive {
  override def toString: String = "NIL"
}
case object DInvalid extends DPrimitive {
  override def toString: String = "[INVALID]"
}
case object DDeadKey extends DPrimitive{
  override def toString: String = "DEAD_KEY"
}

//Tables
sealed abstract class DTable extends DesyncedData

case class DMap(a: Vector[DesyncedData], m: Map[DPrimitive, DesyncedData]) extends DTable {
  given Ordering[Int | String] = Ordering.fromLessThan({
    case (a: Int, b: Int) => a < b
    case (_: Int, _: String) => true
    case (a: String, b: String) => a < b
  })
  def toSimpleMap: SortedMap[Int | String, DesyncedData] =
    SortedMap.empty[Int | String, DesyncedData]
      ++ m.map((k,v) => (k.toString, v))
      ++ a.zipWithIndex.map((v,k) => (k + 1, v)).toMap

}
case class DArray(a: Vector[DesyncedData]) extends DTable

//Special
case class DUserData(data: ByteVector) extends DesyncedData






