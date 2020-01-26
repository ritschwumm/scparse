package scparse.ng.binary

import scutil.lang._

import scparse.ng._

// TODO quote
object BinaryParser {
	def anyByte:BinaryParser[Byte]								= Parser.any							named "any byte"

	def anyByteOf(s:ByteString):BinaryParser[Byte]				= Parser inSet		s.toSet				named s"any byte in ${s.toString}"
	def anyByteInRange(from:Byte, to:Byte):BinaryParser[Byte]	= Parser inRange	(from, to)			named s"any byte between ${from.toString} and ${to.toString}"

	def takeBytes(count:Int):BinaryParser[Seq[Byte]]			= Parser take	count					named s"any ${count.toString} bytes"
	def takeByteString(count:Int):BinaryParser[ByteString]		= takeBytes(count).stringify

	def isByte(c:Byte):BinaryParser[Byte]						= Parser is c							named s"the byte ${c.toString}"
	def isByteString(s:ByteString):BinaryParser[ByteString]		= Parser.isSeq(s.toVector).stringify	named s"the byte string ${s.toString}"
}
