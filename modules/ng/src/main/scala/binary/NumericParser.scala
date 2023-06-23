package scparse.ng.binary

import scutil.lang.*

object NumericParser {
	val int8:BinaryParser[Byte]			= number("int8",		1, _.toByte)

	val int16_be:BinaryParser[Short]	= number("int16 be",	2, _.toBigEndianShort)
	val int32_be:BinaryParser[Int]		= number("int32 be",	4, _.toBigEndianInt)
	val int64_be:BinaryParser[Long]		= number("int64 be",	8, _.toBigEndianLong)

	val int16_le:BinaryParser[Short]	= number("int16 le",	2, _.toLittleEndianShort)
	val int32_le:BinaryParser[Int]		= number("int32 le",	4, _.toLittleEndianInt)
	val int64_le:BinaryParser[Long]		= number("int64 le",	8, _.toLittleEndianLong)

	def number[T](name:String, length:Int, extract:ByteString=>Option[T]):BinaryParser[T]	=
		BinaryParser takeByteString length map extract flattenOptionNamed name
}
