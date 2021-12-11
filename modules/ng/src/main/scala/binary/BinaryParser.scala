package scparse.ng.binary

import scutil.lang.*

import scparse.ng.*

// TODO quote
object BinaryParser {
	def any:BinaryParser[Byte]								= Parser.any							named "any byte"

	def anyOf(s:ByteString):BinaryParser[Byte]				= Parser inSet		s.toSet				named s"any byte in ${s.toString}"
	def noneOf(s:ByteString):BinaryParser[Byte]				= Parser notInSet	s.toSet				named s"any byte except bytes in ${s.toString}"

	def anyIn(from:Byte, to:Byte):BinaryParser[Byte]		= Parser.inRange	(from, to)			named s"any byte between ${from.toString} and ${to.toString}"
	def noneIn(from:Byte, to:Byte):BinaryParser[Byte]		= Parser.notInRange	(from, to)			named s"any byte between except bytes between ${from.toString} and ${to.toString}"

	def take(count:Int):BinaryParser[Seq[Byte]]				= Parser take	count					named s"any ${count.toString} bytes"
	def takeByteString(count:Int):BinaryParser[ByteString]	= take(count).stringify

	def is(c:Byte):BinaryParser[Byte]						= Parser is c							named s"the byte ${c.toString}"
	def isByteString(s:ByteString):BinaryParser[ByteString]	= (Parser isSeq s.toVector).stringify	named s"the byte string ${s.toString}"

	def remainder:BinaryParser[Seq[Byte]]					= Parser.remainder[Byte]
	def remainderByteString:BinaryParser[ByteString]		= remainder.stringify
}
