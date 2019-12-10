package scparse.ng

import scutil.base.implicits._
import scutil.lang._

object ParseResult {
	def AnonFailure[S,T](index:Int):ParseResult[S,T]				= Failure(index, List.empty)
	def LeafFailure[S,T](index:Int, error:String):ParseResult[S,T]	= Failure(index, List(error))

	final case class Success[S,T](tail:ParserInput[S], t:T)			extends ParseResult[S,T]
	final case class Failure[S,T](index:Int, errors:List[String])	extends ParseResult[S,T]
}

sealed trait ParseResult[S,+T] {
	def toOption:Option[T]								= toEither.toOption
	def toValidated:Validated[(Int,List[String]), T]	= toEither.toValidated

	def toEither:Either[(Int,List[String]),T]	=
			this match {
				case ParseResult.Success(_,		t)	=> Right(t)
				case ParseResult.Failure(index, e)	=> Left((index, e))
			}
}
