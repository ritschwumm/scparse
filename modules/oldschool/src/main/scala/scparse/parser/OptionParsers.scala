package scparse.oldschool

import scala.annotation.tailrec
import scala.collection.mutable

/** specials only useful with an Option base */
trait OptionParsers { self:Parsers[Option] =>
	def scanner[C,T](sub:Parser[C,T]):Parser[C,Seq[T]]	=
			Parser { (s:Source[C]) =>
				val out	= mutable.ArrayBuffer.empty[T]
				@tailrec
				def loop(ss:Source[C]):Result[C,Seq[T]]	= {
					sub(ss) match {
						case Some((rest, value))	=>
							out	+= value
							loop(rest)
						case None	=>
							// TODO ugly, but without match we don't get tailrec
							val more	= ss cata (None, (rest,_) => Some(rest))
							more match {
								case Some(x)	=> loop(x)
								case None		=> Some((ss, out.toVector))
							}
					}
				}
				loop(s)
			}
}
