package scparse.ng.binary

import scutil.lang._

import scparse.ng._

// TODO quote
object BinaryParser {
	def anyByte:BinaryParser[Byte]								= Parser.any						named "any byte"

	def anyByteOf(s:ByteString):BinaryParser[Byte]				= Parser acceptSet	s.toSet			named s"any byte in ${s.toString}"
	def anyByteInRange(from:Byte, to:Byte):BinaryParser[Byte]	= Parser acceptRange	(from, to)	named s"any byte between ${from.toString} and ${to.toString}"

	def anyCountBytes(count:Int):BinaryParser[Seq[Byte]]		= Parser anyCount	count			named s"any ${count.toString} bytes"
	def anyCountByteString(count:Int):BinaryParser[ByteString]	= anyCountBytes(count).stringify

	def isByte(c:Byte):BinaryParser[Unit]						= Parser is c						named s"the byte ${c.toString}"
	def isByteString(s:ByteString):BinaryParser[Unit]			= Parser.isSeq(s.toVector)			named s"the byte string ${s.toString}"
}
