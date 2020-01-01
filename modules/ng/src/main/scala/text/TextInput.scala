package scparse.ng.text

import scparse.ng._

object TextInput {
	def of(s:String):ParserInput[Char]	= TextInput(s, 0)
}

final case class TextInput(s:String, index:Int) extends ParserInput[Char] {
	def next:Option[(ParserInput[Char],Char)]	=
		if (index < s.length)	Some(((TextInput(s, index+1)), (s charAt index)))
		else					None
}
