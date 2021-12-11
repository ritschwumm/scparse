package scparse.oldschool

import scala.language.implicitConversions

/** parsers for Strings */
trait StringParsers[M[+_]] { self:Parsers[M] =>
	type StringParser[+T]	= Parser[Char,T]

	implicit def liftString(cs:String):StringParser[String]	= liftSeq(cs) map { _ mkString "" }

	//------------------------------------------------------------------------------

	def symbol(name:String):StringParser[String]		= token(name)
	// def word:StringParser[List[Char]]				= token(anyIf(isPrintable).+)

	def full[T](sub:StringParser[T]):StringParser[T]	= (space.option	right sub).phrase
	def token[T](sub:StringParser[T]):StringParser[T]	= sub		left space.option
	def space:StringParser[List[Char]]					= white.repeat

	//------------------------------------------------------------------------------

	def anyBetween(from:Char, to:Char):StringParser[Char]	= self.any collect { case c if c >= from && c <= to => c }
	def anyCharsInclude(chars:String):StringParser[Char]	= self.anyInclude(chars*)
	def anyCharsExclude(chars:String):StringParser[Char]	= self.anyExclude(chars*)

	lazy val digit:StringParser[Char]	= self satisfy (_.isDigit)
	lazy val letter:StringParser[Char]	= self satisfy (_.isLetter)
	lazy val upper:StringParser[Char]	= self satisfy (_.isUpper)
	lazy val lower:StringParser[Char]	= self satisfy (_.isLower)
	lazy val white:StringParser[Char]	= self satisfy (_.isWhitespace)
	lazy val control:StringParser[Char]	= self satisfy (_.isControl)
}
