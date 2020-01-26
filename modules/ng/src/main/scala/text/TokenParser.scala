package scparse.ng.text

import scparse.ng.text.CharParser._

object TokenParser {
	val Standard	= new TokenParser(white.vector)
}

final class TokenParser(val space:TextParser[Seq[Char]]) {
	final def symbol(name:String):TextParser[String]	= token(TextParser isString name)
	// final def word:TextParser[List[Char]]			= token(anyIf(isPrintable).+)

	final def full[T](sub:TextParser[T]):TextParser[T]	= sub finishLeft space
	final def token[T](sub:TextParser[T]):TextParser[T]	= sub eatRight space
}
