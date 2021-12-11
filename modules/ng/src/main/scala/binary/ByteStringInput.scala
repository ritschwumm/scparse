package scparse.ng.binary

import scutil.lang.*

import scparse.ng.*

object ByteStringInput {
	def of(s:ByteString):ParserInput[Byte]	= ByteStringInput(s, 0)
}

final case class ByteStringInput(s:ByteString, index:Int) extends ParserInput[Byte] {
	def next:Option[(ParserInput[Byte],Byte)]	=
		s get index map { byte =>
			ByteStringInput(s, index+1) -> byte
		}
}
