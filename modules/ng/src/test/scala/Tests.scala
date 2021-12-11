package scparse.ng.demo

import minitest.*

//import scparse.ng.*
import scparse.ng.text.*

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
}
