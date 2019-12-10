package scparse.oldschool

object SeqSource {
	def of[C](input:Seq[C]):SeqSource[C]	= SeqSource(input)
}

final case class SeqSource[C](input:Seq[C]) extends Source[C] {
	def cata[X](empty: =>X, full:(Source[C],C)=>X):X	=
			input match {
				case head +: tail	=> full(SeqSource(tail), head)
				case _				=> empty
			}
}
