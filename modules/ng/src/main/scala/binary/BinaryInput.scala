package scparse.ng.binary

import scutil.lang._

import scparse.ng._

object BinaryInput {
	def of(s:ByteString):ParserInput[Byte]	= BinaryInput(s, 0)
}

final case class BinaryInput(s:ByteString, index:Int) extends ParserInput[Byte] {
	def next:Option[(ParserInput[Byte],Byte)]	=
			s get index map { byte =>
				BinaryInput(s, index+1) -> byte
			}
}
