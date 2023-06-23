package scparse.ng.demo

import scparse.ng.*
import scparse.ng.text.*

object CalculatorDemo {
	def main(args:Array[String]):Unit	= {
		println(complete parseString """4 - 2 * 3 ^ 2""")
	}

	val tokens	= TokenParser.Standard

	lazy val complete	= tokens.full(expression)

	lazy val expression:TextParser[BigInt]
						= term		chainLeft (add orElse sub)
	lazy val term		= factor	chainLeft (mul orElse div)
	lazy val factor		= value		chainRight exp
	lazy val value		= number orElse bracket
	lazy val bracket	= tokens.symbol("(") right Parser.defer(expression) left tokens.symbol(")")

	lazy val add		= binary[BigInt]("+")(_+_)
	lazy val sub		= binary[BigInt]("-")(_-_)
	lazy val mul		= binary[BigInt]("*")(_*_)
	lazy val div		= binary[BigInt]("/")(_/_)
	lazy val exp		= binary[BigInt]("^")(_ pow _.toInt)

	lazy val number		= sign ap natural
	lazy val natural	= tokens.token(NaturalParser.natural)
	lazy val sign		= signPlus orElse signMinus orElse signNone
	lazy val signPlus	= unary[BigInt]("+")(identity)
	lazy val signMinus	= unary[BigInt]("-")(-_)
	lazy val signNone	= nullary[BigInt](identity)

	def nullary[X](func:X=>X):TextParser[X=>X]						= Parser.success(func)
	def unary[X](sym:String)(func:X=>X):TextParser[X=>X]			= tokens.symbol(sym) tag func
	def binary[X](sym:String)(func:(X,X)=>X):TextParser[(X,X)=>X]	= tokens.symbol(sym) tag func
}
