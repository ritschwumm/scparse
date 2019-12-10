package scparse.oldschool

import scala.language.implicitConversions

/** parsers for Strings */
trait StringParsers[M[+_]] { self:Parsers[M] =>
	type StringParser[+T]	= Parser[Char,T]

	implicit def liftString(cs:String):StringParser[String]	= liftSeq(cs) map { _ mkString "" }

	//------------------------------------------------------------------------------

	def symbol(name:String):StringParser[String]		= token(name)
	// def word:StringParser[List[Char]]				= token(anyIf(isPrintable).+)

	def full[T](sub:StringParser[T]):StringParser[T]	= (space.?	~> sub).$
	def token[T](sub:StringParser[T]):StringParser[T]	= sub		<~ space.?
	def space:StringParser[List[Char]]					= white.+

	//------------------------------------------------------------------------------

	def anyBetween(from:Char,to:Char):StringParser[Char]	=
			self.any ^? { case c if c >= from && c <= to => c }
	def anyCharsInclude(chars:String):StringParser[Char]	=
			self anyInclude (chars:_*)
	def anyCharsExclude(chars:String):StringParser[Char]	=
			self anyExclude (chars:_*)

	lazy val digit:StringParser[Char]	= self anyIf isDigit
	lazy val letter:StringParser[Char]	= self anyIf isLetter
	lazy val upper:StringParser[Char]	= self anyIf isUpper
	lazy val lower:StringParser[Char]	= self anyIf isLower
	lazy val white:StringParser[Char]	= self anyIf isWhitespace
	lazy val control:StringParser[Char]	= self anyIf isControl

	private val isDigit:Char=>Boolean		= _.isDigit
	private val isLetter:Char=>Boolean		= _.isLetter
	private val isUpper:Char=>Boolean		= _.isUpper
	private val isLower:Char=>Boolean		= _.isLower
	private val isWhitespace:Char=>Boolean	= _.isWhitespace
	private val isControl:Char=>Boolean		= _.isControl

	// private val isPrintable:Char=>Boolean	= it => !(it.isWhitespace || it.isControl)
}
