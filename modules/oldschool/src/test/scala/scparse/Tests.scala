package scparse.oldschool.demo

import minitest.*

object Tests extends SimpleTestSuite {
	test("calculator demoshould just work") {
		assertEquals(
			CalculatorDemo.TestParsers.complete parse """4 - 2 * 3 ^ 2""" map { _._2 },
			Some(-14)
		)
	}

	test("simple demo should just work") {
		assertEquals(
			SimpleDemo.TestParsers.foo parsePhrase "aab",
			Some((List('a','a'),List('b')))
		)
	}

	test("json demo should just work") {
		assertEquals(
			JSONDemo.TestParsers.json parse "-2.3E+10" map { _._2 },
			Some(JSONNumber(-2.3E+10))
		)
	}
}
