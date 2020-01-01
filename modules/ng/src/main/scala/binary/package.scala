package scparse.ng

import scutil.lang._

package object binary {
	type BinaryParser[T]	= Parser[Byte,T]

	//------------------------------------------------------------------------------

	implicit class BinaryParserNestOps(peer:BinaryParser[ByteString]) {
		def nestBinaryString[T](inner:BinaryParser[T]):Parser[Byte,T]	=
			peer nest (BinaryInput.of, inner)
	}

	implicit class BinaryParserParseOps[T](peer:BinaryParser[T]) {
		def parseByteString(s:ByteString):ParserResult[Byte,T]	=
			peer parse (BinaryInput of s)
	}

	implicit class BinaryParserStringifySeqOps[T](peer:Parser[T,Seq[Byte]]) {
		def stringify:Parser[T,ByteString]	=
			peer map ByteString.fromSeq
	}

	implicit class BinaryParserStringifyNesOps[T](peer:Parser[T,Nes[Byte]]) {
		def stringify:Parser[T,ByteString]	=
			peer map { it => ByteString fromSeq it.toSeq }
	}
}
