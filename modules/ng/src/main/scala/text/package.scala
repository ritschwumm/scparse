package scparse.ng.text

import scala.annotation.targetName

import scutil.lang.*

import scparse.ng.*


type TextParser[T]	= Parser[Char,T]

//------------------------------------------------------------------------------

extension (peer:TextParser[String]) {
	def nestString[T](inner:Parser[Char,T]):TextParser[T]	=
		peer.nest(StringInput.of, inner)
}

extension [T](peer:Parser[Char,T]) {
	def parseString(s:String):ParserResult[Char,T]	=
		peer parse (StringInput of s)
}

extension [T](peer:Parser[T,Seq[Char]]) {
	@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
	@targetName("stringifySeq")
	def stringify:Parser[T,String]	=
		peer map { _.mkString }
}

extension [T](peer:Parser[T,Nes[Char]]) {
	@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
	@targetName("stringifyNes")
	def stringify:Parser[T,String]	=
		peer map { _.toSeq.mkString }
}
