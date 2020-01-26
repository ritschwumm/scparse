package scparse.ng.text

import scparse.ng._

// TODO quote
object TextParser {
	def any:TextParser[Char]						= Parser.any							named "any char"

	def anyOf(s:String):TextParser[Char]			= Parser inSet		s.toSet				named s"any char in ${s}"
	def noneOf(s:String):TextParser[Char]			= Parser notInSet	s.toSet				named s"any char except chars in ${s}"

	def anyIn(from:Char, to:Char):TextParser[Char]	= Parser inRange	(from, to)			named s"any char between ${from.toString} and ${to.toString}"
	def noneIn(from:Char, to:Char):TextParser[Char]	= Parser notInRange	(from, to)			named s"any char except chars between ${from.toString} and ${to.toString}"

	def take(count:Int):TextParser[Seq[Char]]		= Parser take	count					named s"any ${count.toString} chars"
	def takeString(count:Int):TextParser[String]	= take(count).stringify

	def is(c:Char):TextParser[Char]					= Parser is c							named s"the char ${c.toString}"
	def isString(s:String):TextParser[String]		= (Parser isSeq s.toVector).stringify	named s"the string ${s}"

	def remainder:TextParser[Seq[Char]]				= Parser.remainder[Char]
	def remainderString:TextParser[String]			= remainder.stringify
}
