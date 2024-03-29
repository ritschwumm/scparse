package scparse.ng

import scutil.core.implicits.*
import scutil.lang.*

object ParserResult {
	def AnonFailure[S](index:Int):ParserResult[S,Nothing]				= Failure(index, List.empty)
	def LeafFailure[S](index:Int, error:String):ParserResult[S,Nothing]	= Failure(index, List(error))
}

enum ParserResult[S,+T] {
	case Success[S,T](tail:ParserInput[S], t:T)			extends ParserResult[S,T]
	case Failure[S](index:Int, errors:List[String])		extends ParserResult[S,Nothing]

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
