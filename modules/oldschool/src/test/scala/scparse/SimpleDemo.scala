package scparse.oldschool.demo

import scparse.oldschool._

object SimpleDemo {
	def main(args:Array[String]):Unit	= {
		println(TestParsers.foo parse "aab")
	}

	object TestParsers extends Parsers[Option] {
		def foo	= repeat('a') ~ repeat(any)
	}
}
