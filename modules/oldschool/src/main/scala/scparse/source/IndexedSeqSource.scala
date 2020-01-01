package scparse.oldschool

object IndexedSeqSource {
	def of[C](input:IndexedSeq[C]):IndexedSeqSource[C]	= IndexedSeqSource(input, 0)
}

final case class IndexedSeqSource[C](input:IndexedSeq[C], position:Int) extends Source[C] {
	def cata[X](empty: =>X, full:(Source[C],C)=>X):X	=
		if (position < input.size)	full(IndexedSeqSource(input, position+1), input apply position)
		else						empty
}
