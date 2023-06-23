package scparse.ng.binary

import scala.annotation.targetName

import scutil.lang.*

import scparse.ng.*

type BinaryParser[T]	= Parser[Byte,T]

//------------------------------------------------------------------------------

extension (peer:BinaryParser[ByteString]) {
	def nestByteString[T](inner:BinaryParser[T]):Parser[Byte,T]	=
		peer.nest(ByteStringInput.of, inner)
}

extension [T](peer:BinaryParser[T]) {
	def parseByteString(s:ByteString):ParserResult[Byte,T]	=
		peer parse (ByteStringInput of s)
}

extension [T](peer:Parser[T,Seq[Byte]]) {
	@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
	@targetName("stringifySeq")
	def stringify:Parser[T,ByteString]	=
		peer map ByteString.fromIterable
}

extension [T](peer:Parser[T,Nes[Byte]]) {
	@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
	@targetName("stringifyNes")
	def stringify:Parser[T,ByteString]	=
		peer map { it => ByteString fromIterable it.toSeq }
}
