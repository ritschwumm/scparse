package scparse.ng.seq

import scparse.ng._

object IndexedSeqInput {
	def of[T](s:IndexedSeq[T]):ParserInput[T]	= IndexedSeqInput(s, 0)
}

final case class IndexedSeqInput[T](s:IndexedSeq[T], index:Int) extends ParserInput[T] {
	def next:Option[(ParserInput[T],T)]	=
		s lift index map { item =>
			IndexedSeqInput(s, index+1) -> item
		}
}
