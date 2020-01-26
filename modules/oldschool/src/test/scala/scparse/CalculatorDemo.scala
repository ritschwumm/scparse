package scparse.oldschool.demo

import scparse.oldschool._

object CalculatorDemo {
	def main(args:Array[String]):Unit	= {
		println(TestParsers.complete parse """4 - 2 * 3 ^ 2""")
	}

	object TestParsers extends Parsers[Option] with StringParsers[Option] with NaturalParsers[Option] {
		def complete	= full(expression)

		def expression:StringParser[BigInt]
					= term		chainLeft (add alternate sub)
		def term	= factor	chainLeft (mul alternate div)
		def factor	= value		chainRight exp
		def value	= number alternate bracket
		def bracket	= symbol("(") right expression left symbol(")")

		def add		= binary[BigInt]("+")(_+_)
		def sub		= binary[BigInt]("-")(_-_)
		def mul		= binary[BigInt]("*")(_*_)
		def div		= binary[BigInt]("/")(_/_)
		def exp		= binary[BigInt]("^")(_ pow _.toInt)

		def number		= sign pa token(natural)
		def sign		= signPlus alternate signMinus alternate signNone
		def signPlus	= unary[BigInt]("+")(identity)
		def signMinus	= unary[BigInt]("-")(-_)
		def signNone	= nullary[BigInt](identity)

		def nullary[X](func:X=>X):StringParser[X=>X]					= success(func)
		def unary[X](sym:String)(func:X=>X):StringParser[X=>X]			= symbol(sym) tag func
		def binary[X](sym:String)(func:(X,X)=>X):StringParser[(X,X)=>X]	= symbol(sym) tag func
	}
}
