package scparse.ng.demo

import scparse.ng.*
import scparse.ng.text.*

object JsonDemo {
	def main(args:Array[String]):Unit	= {
		//val str	= "\"\\u0041\""
		//val str	= """-10.707e+1"""
		val str	= "-2.3E+10"
		val res	= json parseString str
		println(res)
	}

	import Parser.success
	import Parser.defer
	import TextParser.any
	import TextParser.is
	import TextParser.isString
	import TextParser.anyOf
	import TextParser.anyIn
	import NaturalParser.natural
	import NaturalParser.digit
	import NaturalParser.buildNumber
	import NaturalParser.decodeNumber
	import TokenParser.Standard.symbol

	lazy val json:TextParser[Json]	= value.phrase

	// nonterminals
	lazy val value:TextParser[Json]			= (obj:TextParser[Json]) orElse arr orElse str orElse num orElse tru orElse fls orElse nul
	lazy val arr:TextParser[Json.Array]		= is('[') right (defer(value)	seqSepBy is(',')) left is(']')	map Json.Array.apply
	lazy val obj:TextParser[Json.Object]	= is('{') right (defer(elm)		seqSepBy is(',')) left is('}')	map Json.Object.apply
	lazy val elm:TextParser[(String,Json)]	= (stringLit left is(':')) next value	// OR str ~ (':' right value)
	lazy val str:TextParser[Json.String]	= stringLit	map Json.String.apply
	lazy val num:TextParser[Json.Number]	= numberLit	map Json.Number.apply
	lazy val tru:TextParser[Json.Boolean]	= isString("true")	tag Json.Boolean(true)
	lazy val fls:TextParser[Json.Boolean]	= isString("false")	tag Json.Boolean(false)
	lazy val nul:TextParser[Json]			= isString("null")	tag Json.Null

	// string
	lazy val stringLit:TextParser[String]	= {
		lazy val stringChar:TextParser[Char]	= is('"').not right is('\\').not right any
		lazy val stringEsc:TextParser[Char]		= is('\\') right (
													anyOf("\"\\/") orElse
													(is('b') tag '\b') orElse
													(is('f') tag '\f') orElse
													(is('n') tag '\n') orElse
													(is('r') tag '\r') orElse
													(is('t') tag '\t') orElse
													(is('u') right hexChar)
												)
		lazy val hexChar:TextParser[Char]	= (digit orElse anyIn('a','f')) times 4 map { buildNumber(16, decodeNumber, _).toChar }
		is('"') right (stringEsc  orElse stringChar).vector left is('"') map { _ mkString "" }
	}

	// number
	lazy val numberLit:TextParser[BigDecimal]	= {
		val signBD:TextParser[BigDecimal=>BigDecimal]	= {
			val minus:TextParser[BigDecimal=>BigDecimal]	= symbol("-") tag (-_)
			val none:TextParser[BigDecimal=>BigDecimal]		= success(identity)
			(minus orElse none)
		}

		def body:TextParser[BigInt]	= natural

		def frac:TextParser[(BigInt,Int)]		= {
			def ok:TextParser[(BigInt,Int)]	= success((BigInt(0),0))
			def nums:TextParser[(BigInt,Int)]	=
				digit.nes map { it =>
					(buildNumber(10, decodeNumber, it.toSeq), it.size)
				}
			is('.') right (nums orElse ok) orElse ok
		}

		val signBI:TextParser[BigInt=>BigInt]	= {
			val plus:TextParser[BigInt=>BigInt]	= symbol("+") tag identity
			val minus:TextParser[BigInt=>BigInt]	= symbol("-") tag (-_)
			val none:TextParser[BigInt=>BigInt]	= success(identity)
			(plus orElse minus orElse none)
		}

		def exp:TextParser[Int]	= {
			def ok:TextParser[Int]	= success(0)
			(anyOf("eE") right signBI ap natural map { _.toInt }) orElse ok
		}

		def signLess:TextParser[BigDecimal]	= (body next frac next exp) map { case ((b, (fb, fe)), e)	=>
			BigDecimal(b, -e) + BigDecimal(fb, fe-e)
		}

		signBD ap signLess
	}
}
