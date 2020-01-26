package scparse.ng.seq

import scparse.ng._

object ListInput {
	def of[T](s:List[T]):ParserInput[T]	= ListInput(s, 0)
}

final case class ListInput[T](s:List[T], index:Int) extends ParserInput[T] {
	def next:Option[(ParserInput[T],T)]	=
		s match {
			case h :: t	=> Some(ListInput(t, index+1) -> h)
			case Nil	=> None
		}
}
