package scparse.oldschool.demo

import org.specs2.mutable._

class Tests extends Specification {
	"calculator demo" should {
		"just work" in {
			CalculatorDemo.TestParsers.complete parse """4 - 2 * 3 ^ 2""" map { _._2 } mustEqual Some(-14)
		}
	}

	"simple demo" should {
		"just work" in {
			SimpleDemo.TestParsers.foo parsePhrase "aab" mustEqual Some((List('a','a'),List('b')))
		}
	}

	"json demo" should {
		"just work" in {
			JSONDemo.TestParsers.json parse "-2.3E+10" map { _._2 } mustEqual Some(JSONNumber(-2.3E+10))
		}
	}
}
