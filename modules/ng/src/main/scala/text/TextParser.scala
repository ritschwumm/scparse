package scparse.ng.text

import scparse.ng._

// TODO quote
object TextParser {
	def anyChar:TextParser[Char]							= Parser.any							named "any char"

	def anyCharOf(s:String):TextParser[Char]				= Parser inSet		s.toSet				named s"any char in ${s}"
	def anyCharInRange(from:Char, to:Char):TextParser[Char]	= Parser inRange	(from, to)			named s"any char between ${from.toString} and ${to.toString}"

	def anyCountChars(count:Int):TextParser[Seq[Char]]		= Parser anyCount	count				named s"any ${count.toString} chars"
	def anyCountString(count:Int):TextParser[String]		= anyCountChars(count).stringify

	def isChar(c:Char):TextParser[Char]						= Parser is c							named s"the char ${c.toString}"
	def isString(s:String):TextParser[String]				= Parser.isSeq(s.toVector).stringify	named s"the string ${s}"
}
