package scparse.ng.text

import scparse.ng._

object StringInput {
	def of(s:String):ParserInput[Char]	= StringInput(s, 0)
}

final case class StringInput(s:String, index:Int) extends ParserInput[Char] {
	def next:Option[(ParserInput[Char],Char)]	=
		if (index < s.length)	Some(((StringInput(s, index+1)), (s charAt index)))
		else					None
}
