package scparse.oldschool.demo

import scparse.oldschool.*

object SimpleDemo {
	def main(args:Array[String]):Unit	= {
		println(TestParsers.foo parsePhrase "aab")
	}

	object TestParsers extends Parsers[Option] {
		def foo	= repeat('a') next repeat(any)
	}
}
