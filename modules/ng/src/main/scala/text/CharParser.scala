package scparse.ng.text

import scparse.ng._

object CharParser {
	val digit:TextParser[Char]		= Parser.satisfy[Char] (_.isDigit)		named "digit"
	val letter:TextParser[Char]		= Parser.satisfy [Char](_.isLetter)		named "letter"
	val upper:TextParser[Char]		= Parser.satisfy[Char] (_.isUpper)		named "upper"
	val lower:TextParser[Char]		= Parser.satisfy[Char] (_.isLower)		named "lower"
	val white:TextParser[Char]		= Parser.satisfy[Char] (_.isWhitespace)	named "white"
	val control:TextParser[Char]	= Parser.satisfy[Char] (_.isControl)	named "control"
}
