package scparse.oldschool.demo

import scparse.oldschool.*

object JSONDemo {
	def main(args:Array[String]):Unit	= {
		//val str	= "\"\\u0041\""
		//val str	= """-10.707e+1"""
		val str	= "-2.3E+10"
		val res	= TestParsers.json parse str
		println(res)
	}

	object TestParsers extends Parsers[Option] with StringParsers[Option] with NaturalParsers[Option] {
		lazy val json:StringParser[JSONValue]	= value.phrase

		// nonterminals
		lazy val value:StringParser[JSONValue]			= (obj:StringParser[JSONValue]) alternate arr alternate str alternate num alternate tru alternate fls alternate nul
		lazy val arr:StringParser[JSONArray]			= '[' right (value repeatSeparated1 ',') left ']'	map JSONArray.apply
		lazy val obj:StringParser[JSONObject]			= '{' right (elm  repeatSeparated1 ',') left '}'	map JSONObject.apply
		lazy val elm:StringParser[(String,JSONValue)]	= (stringLit left ':') next value	// OR str ~ (':' right value)
		lazy val str:StringParser[JSONString]			= stringLit	map JSONString.apply
		lazy val num:StringParser[JSONNumber]			= numberLit	map JSONNumber.apply
		lazy val tru:StringParser[JSONBoolean]			= "true"	tag JSONBoolean(true)
		lazy val fls:StringParser[JSONBoolean]			= "false"	tag JSONBoolean(false)
		lazy val nul:StringParser[JSONValue]			= "null"	tag JSONNull

		// string
		lazy val stringLit:StringParser[String]	= {
			lazy val stringChar:StringParser[Char]	= '"'.not right '\\'.not right any
			lazy val stringEsc:StringParser[Char]	= '\\' right (
													anyInclude('"', '\\', '/') alternate
													('b' tag '\b') alternate
													('f' tag '\f') alternate
													('n' tag '\n') alternate
													('r' tag '\r') alternate
													('t' tag '\t') alternate
													('u' right hexChar))
			lazy val hexChar:StringParser[Char]	= (digit alternate anyBetween('a','f')) repeatN 4 map { buildNumber(16, decodeNumber, _).toChar }
			'"' right (stringEsc  alternate stringChar).repeat left '"' map { _ mkString "" }
		}

		// number
		lazy val numberLit:StringParser[BigDecimal]	= {
			val signBD:StringParser[BigDecimal=>BigDecimal]	= {
				val minus:StringParser[BigDecimal=>BigDecimal]	= symbol("-") tag (-_)
				val none:StringParser[BigDecimal=>BigDecimal]	= success(identity)
				(minus alternate none)
			}

			def body:StringParser[BigInt]	= natural

			def frac:StringParser[(BigInt,Int)]		= {
				def ok:StringParser[(BigInt,Int)]	= success((BigInt(0),0))
				def nums:StringParser[(BigInt,Int)]	= digit.repeat1 map { it =>
					(buildNumber(10, decodeNumber, it), it.size)
				}
				'.' right (nums alternate ok) alternate ok
			}

			val signBI:StringParser[BigInt=>BigInt]	= {
				val plus:StringParser[BigInt=>BigInt]	= symbol("+") tag identity
				val minus:StringParser[BigInt=>BigInt]	= symbol("-") tag (-_)
				val none:StringParser[BigInt=>BigInt]	= success(identity)
				(plus alternate minus alternate none)
			}

			def exp:StringParser[Int]	= {
				def ok:StringParser[Int]	= success(0)
				(anyInclude('e', 'E') right signBI ap natural map { _.toInt }) alternate ok
			}

			def signLess:StringParser[BigDecimal]	= (body next frac next exp) map { case ((b, (fb, fe)), e)	=>
				BigDecimal(b, -e) + BigDecimal(fb, fe-e)
			}

			signBD ap signLess
		}
	}
}
