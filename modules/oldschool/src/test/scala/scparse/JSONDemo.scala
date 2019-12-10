package scparse.oldschool.demo

import scparse.oldschool._

object JSONDemo {
	def main(args:Array[String]):Unit	= {
		//val str	= "\"\\u0041\""
		//val str	= """-10.707e+1"""
		val str	= "-2.3E+10"
		val res	= TestParsers json str
		println(res)
	}

	object TestParsers extends Parsers[Option] with StringParsers[Option] with NaturalParsers[Option] {
		lazy val json:StringParser[JSONValue]	= value.$

		// nonterminals
		lazy val value:StringParser[JSONValue]			= (obj:StringParser[JSONValue]) | arr | str | num | tru | fls | nul
		lazy val arr:StringParser[JSONArray]			= '[' ~> (value *% ',') <~ ']'	^^ JSONArray.apply
		lazy val obj:StringParser[JSONObject]			= '{' ~> (elm  *% ',') <~ '}'	^^ JSONObject.apply
		lazy val elm:StringParser[(String,JSONValue)]	= (stringLit <~ ':') ~ value	// OR str ~ (':' ~> value)
		lazy val str:StringParser[JSONString]			= stringLit	^^ JSONString.apply
		lazy val num:StringParser[JSONNumber]			= numberLit	^^ JSONNumber.apply
		lazy val tru:StringParser[JSONBoolean]			= "true"	^^^ JSONBoolean(true)
		lazy val fls:StringParser[JSONBoolean]			= "false"	^^^ JSONBoolean(false)
		lazy val nul:StringParser[JSONValue]			= "null"	^^^ JSONNull

		// string
		lazy val stringLit:StringParser[String]	= {
			lazy val stringChar:StringParser[Char]	= !'"' ~> !'\\' ~> any
			lazy val stringEsc:StringParser[Char]	= '\\' ~> (
													anyInclude('"', '\\', '/') |
													('b' ^^^ '\b') |
													('f' ^^^ '\f') |
													('n' ^^^ '\n') |
													('r' ^^^ '\r') |
													('t' ^^^ '\t') |
													('u' ~> hexChar))
			lazy val hexChar:StringParser[Char]	= (digit | anyBetween('a','f')) *# 4 map { buildNumber(16, decodeNumber, _).toChar }
			'"' ~> (stringEsc  | stringChar).* <~ '"' ^^ { _ mkString "" }
		}

		// number
		lazy val numberLit:StringParser[BigDecimal]	= {
			val signBD:StringParser[BigDecimal=>BigDecimal]	= {
				val minus:StringParser[BigDecimal=>BigDecimal]	= symbol("-") ^^^ (-_)
				val none:StringParser[BigDecimal=>BigDecimal]	= success(identity)
				(minus | none)
			}

			def body:StringParser[BigInt]	= natural

			def frac:StringParser[(BigInt,Int)]		= {
				def ok:StringParser[(BigInt,Int)]	= success((BigInt(0),0))
				def nums:StringParser[(BigInt,Int)]	= digit.+ ^^ { it =>
					(buildNumber(10, decodeNumber, it), it.size)
				}
				'.' ~> (nums | ok) | ok
			}

			val signBI:StringParser[BigInt=>BigInt]	= {
				val plus:StringParser[BigInt=>BigInt]	= symbol("+") ^^^ identity
				val minus:StringParser[BigInt=>BigInt]	= symbol("-") ^^^ (-_)
				val none:StringParser[BigInt=>BigInt]	= success(identity)
				(plus | minus | none)
			}

			def exp:StringParser[Int]	= {
				def ok:StringParser[Int]	= success(0)
				(anyInclude('e', 'E') ~> signBI <*> natural ^^ { _.toInt }) | ok
			}

			def signLess:StringParser[BigDecimal]	= (body ~ frac ~ exp) map { case ((b, (fb, fe)), e)	=>
				BigDecimal(b, -e) + BigDecimal(fb, fe-e)
			}

			signBD <*> signLess
		}
	}
}
