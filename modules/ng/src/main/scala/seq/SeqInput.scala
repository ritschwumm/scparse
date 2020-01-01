package scparse.ng.seq

import scparse.ng._

object SeqInput {
	def of[T](s:Seq[T]):ParserInput[T]	= SeqInput(s, 0)
}

final case class SeqInput[T](s:Seq[T], index:Int) extends ParserInput[T] {
	def next:Option[(ParserInput[T],T)]	=
		s lift index map { item =>
			SeqInput(s, index+1) -> item
		}
}
