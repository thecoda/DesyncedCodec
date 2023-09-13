package net.thecoda.desynced.codec

import net.thecoda.desynced.codec.DesyncedTableCodec.dfixArray
import scodec.bits.*

// Can't use enum here because it's a nested structure
sealed abstract class DesyncedData

//primitives
sealed abstract class DPrimitive extends DesyncedData

case class DPositiveFixInt(i: Int) extends DPrimitive
case class DNegativeFixInt(i: Int) extends DPrimitive

case class DFloat32(f: Float) extends DPrimitive
case class DFloat64(d: Double) extends DPrimitive

case class DUInt8(i: Int) extends DPrimitive
case class DUInt16(i: Int) extends DPrimitive
case class DUInt32(i: Long) extends DPrimitive
case class DUInt64(i: Long) extends DPrimitive

case class DInt8(i: Int) extends DPrimitive
case class DInt16(i: Int) extends DPrimitive
case class DInt32(i: Int) extends DPrimitive
case class DInt64(i: Long) extends DPrimitive

case class DFixString(s: String) extends DPrimitive
case class DString8(s: String) extends DPrimitive
case class DString16(s: String) extends DPrimitive
case class DString32(s: String) extends DPrimitive

sealed abstract class DBool(val value: Boolean) extends DPrimitive
case object DFalse extends DBool(false)
case object DTrue extends DBool(true)

case object DNil extends DPrimitive
case object DInvalid extends DPrimitive
case object DDeadKey extends DPrimitive

//Tables
sealed abstract class DTable extends DesyncedData

case class DMap(a: Vector[DesyncedData], m: Map[DPrimitive, DesyncedData]) extends DTable
case class DArray(a: Vector[DesyncedData]) extends DTable

//Special
case class DUserData(data: ByteVector) extends DesyncedData






