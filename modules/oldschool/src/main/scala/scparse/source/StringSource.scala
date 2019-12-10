package scparse.oldschool

import scutil.lang.implicits._

object StringSource {
	def of(input:String):StringSource	= StringSource(input, 0)
}

final case class StringSource(input:String, position:Int) extends Source[Char] {
	def cata[X](empty: =>X, full:(Source[Char],Char)=>X):X	=
			if (position < input.length)	full(StringSource(input, position+1), input charAt position)
			else							empty

	override def toString:String	=
			show"StringSource(position=${position}, size=${input.length})"
}
