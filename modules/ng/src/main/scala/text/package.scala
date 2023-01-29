package scparse.ng.text

import scutil.lang.*

import scparse.ng.*

type TextParser[T]	= Parser[Char,T]

//------------------------------------------------------------------------------

implicit final class TextParserNestOps(peer:TextParser[String]) {
	def nestString[T](inner:Parser[Char,T]):TextParser[T]	=
		peer.nest(StringInput.of, inner)
}

implicit final class TextParserParseOps[T](peer:Parser[Char,T]) {
	def parseString(s:String):ParserResult[Char,T]	=
		peer parse (StringInput of s)
}

implicit final class TextParserStringifySeqOps[T](peer:Parser[T,Seq[Char]]) {
	def stringify:Parser[T,String]	=
		peer map { _.mkString }
}

implicit final class TextParserStringifyNesOps[T](peer:Parser[T,Nes[Char]]) {
	def stringify:Parser[T,String]	=
		peer map { _.toSeq.mkString }
}
