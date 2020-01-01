package scparse.ng

import scutil.base.implicits._
import scutil.lang._

object ParserResult {
	def AnonFailure[S](index:Int):ParserResult[S,Nothing]				= Failure(index, List.empty)
	def LeafFailure[S](index:Int, error:String):ParserResult[S,Nothing]	= Failure(index, List(error))

	final case class Success[S,T](tail:ParserInput[S], t:T)			extends ParserResult[S,T]
	final case class Failure[S](index:Int, errors:List[String])		extends ParserResult[S,Nothing]
}

sealed trait ParserResult[S,+T] {
	def toOption:Option[T]								= toEither.toOption
	def toValidated:Validated[(Int,List[String]), T]	= toEither.toValidated

	def toEither:Either[(Int,List[String]),T]	=
		this match {
			case ParserResult.Success(_,		value)	=> Right(value)
			case ParserResult.Failure(index, errors)	=> Left((index, errors))
		}

	@SuppressWarnings(Array("org.wartremover.warts.ToString"))
	override def toString():String	=
		this match {
			case ParserResult.Success(input,	value)	=>
				so"Success at index ${input.index.toString}: ${value.toString})"
			case ParserResult.Failure(index, errors)	=>
				so"Failure at index ${index.toString}" +
				errors.mkString("\t\n", "\t\n", "\n")
		}
}
