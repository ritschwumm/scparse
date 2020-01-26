package scparse.ng.text

import scutil.base.implicits._

import scparse.ng.text.CharParser._

object TokenParser {
	val Standard	= new TokenParser(white.vector)
}

final class TokenParser(val space:TextParser[Seq[Char]]) {
	def symbol(name:String):TextParser[String]	= token(TextParser isString name)	named so"symbol($name)"
	// final def word:TextParser[List[Char]]			= token(anyIf(isPrintable).+)

	def full[T](sub:TextParser[T]):TextParser[T]	= sub finishLeft space
	def token[T](sub:TextParser[T]):TextParser[T]	= sub eatRight space
}
