package scparse.ng.demo

import scparse.ng.*
import scparse.ng.text.*

object SimpleDemo {
	def main(args:Array[String]):Unit	= {
		println(foo.phrase parseString "aab")
	}

	lazy val foo	= Parser.is('a').vector next Parser.any.vector
}
