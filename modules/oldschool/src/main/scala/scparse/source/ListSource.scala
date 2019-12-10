package scparse.oldschool

object ListSource {
	def of[C](input:List[C]):ListSource[C]	= ListSource(input)
}

final case class ListSource[C](delegate:List[C]) extends Source[C] {
	def cata[X](empty: =>X, full:(Source[C],C)=>X):X	=
			delegate match {
				case head :: tail	=> full(ListSource(tail), head)
				case Nil			=> empty
			}
}
