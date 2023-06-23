package scparse.ng.demo

import minitest.*

//import scparse.ng.*
import scparse.ng.text.*
import scparse.ng.ParserResult

object Tests extends SimpleTestSuite {
	test("calculator demo should just work") {
		assertEquals(
			CalculatorDemo.complete.parseString("""4 - 2 * 3 ^ 2""").toOption,
			Some(-14)
		)
	}

	test("simple demo should just work") {
		assertEquals(
			SimpleDemo.foo.phrase.parseString("aab").toOption,
			Some((List('a','a'),List('b')))
		)
	}

	test("json demo should just work") {
		assertEquals(
			JsonDemo.json.parseString("-2.3E+10").toOption,
			Some(Json.Number(-2.3E+10))
		)
	}

	test("padded parsing should work within limit") {
		val input	= StringInput.of("0123456789")
		val parser	=
			for {
				_		<-	TextParser.take(3).padTo(5)
				rest	<-	TextParser.remainderString
			}
			yield rest

		parser.phrase.parse(input) match {
			case ParserResult.Success(tail, value)		=> assertEquals(value, "56789")
			case ParserResult.Failure(index, errors)	=> fail(s"failed at ${index}: ${errors}")
		}
	}

	test("padded parsing should fail outside limit") {
		val input	= StringInput.of("0123456789")
		val parser	=
			for {
				_		<-	TextParser.take(5).padTo(3)
				rest	<-	TextParser.remainderString
			}
			yield rest

		parser.phrase.parse(input) match {
			case ParserResult.Success(tail, value)		=> fail(s"expected a failure")
			case ParserResult.Failure(index, errors)	=> assertEquals(index, 5)
		}
	}
}
