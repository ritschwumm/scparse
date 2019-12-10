package scparse.oldschool

import scutil.base.implicits._

object Base {
/** PEG-style always taking the longes alternative on repetitions */
	implicit object OptionBase extends Base[Option] {
		type M[+X]	= Option[X]

		def zeroM[T]:M[T]									= None
		def unitM[T](in:T):M[T]								= Some(in)

		def optionM[T](in:Option[T]):M[T]					= in
		def iterableM[T](in:Iterable[T]):M[T]				= in.headOption

		def isEmptyM[T](in:M[T]):Boolean					= in.isEmpty

		def filterM[T](in:M[T], predicate:T=>Boolean):M[T]	= in filter		predicate
		def mapM[S,T](in:M[S], func:S=>T):M[T]				= in map		func
		def flatMapM[S,T](in:M[S], func:S=>M[T]):M[T]		= in flatMap	func

		def alternateM[T](left:M[T], right:M[T]):M[T]		= left orElse right
		def preferM[T](left:M[T], right:M[T]):M[T]			= left orElse right
	}

	/** general style returning all possible parses */
	implicit object ListBase extends Base[List] {
		type M[+X]	= List[X]

		def zeroM[T]:M[T]									= List.empty
		def unitM[T](in:T):M[T]								= List(in)

		def optionM[T](in:Option[T]):M[T]					= in.toList
		def iterableM[T](in:Iterable[T]):M[T]				= in.toList

		def isEmptyM[T](in:M[T]):Boolean					= in.isEmpty

		def filterM[T](in:M[T], predicate:T=>Boolean):M[T]	= in filter		predicate
		def mapM[S,T](in:M[S], func:S=>T):M[T]				= in map		func
		def flatMapM[S,T](in:M[S], func:S=>M[T]):M[T]		= in flatMap	func

		def alternateM[T](left:M[T], right:M[T]):M[T]		= left ++ right
		def preferM[T](left:M[T], right:M[T]):M[T]			= if (left.nonEmpty) left else right
	}

	/** general style returning all possible parses */
	implicit object VectorBase extends Base[Vector] {
		type M[+X]	= Vector[X]

		def zeroM[T]:M[T]									= Vector.empty
		def unitM[T](in:T):M[T]								= Vector(in)

		def optionM[T](in:Option[T]):M[T]					= in.toVector
		def iterableM[T](in:Iterable[T]):M[T]				= in.toVector

		def isEmptyM[T](in:M[T]):Boolean					= in.isEmpty

		def filterM[T](in:M[T], predicate:T=>Boolean):M[T]	= in filter		predicate
		def mapM[S,T](in:M[S], func:S=>T):M[T]				= in map		func
		def flatMapM[S,T](in:M[S], func:S=>M[T]):M[T]		= in flatMap	func

		def alternateM[T](left:M[T], right:M[T]):M[T]		= left ++ right
		def preferM[T](left:M[T], right:M[T]):M[T]			= if (left.nonEmpty) left else right
	}
}

/** very similar to MonadPlus, but supporting two different append operations */
trait Base[M[+_]] {
	def zeroM[T]:M[T]
	def unitM[T](in:T):M[T]

	def optionM[T](in:Option[T]):M[T]
	def iterableM[T](in:Iterable[T]):M[T]

	def isEmptyM[T](in:M[T]):Boolean
	final def nonEmptyM[T](in:M[T]):Boolean	= !isEmptyM(in)

	def filterM[T](in:M[T], predicate:T=>Boolean):M[T]
	def mapM[S,T](in:M[S], func:S=>T):M[T]
	// def applicM[S,T](in:M[S], ap:M[S=>T]):M[T]
	def flatMapM[S,T](in:M[S], func:S=>M[T]):M[T]

	def alternateM[T](left:M[T], right:M[T]):M[T]
	def preferM[T](left:M[T], right:M[T]):M[T]

	final def filterMapM[S,T](in:M[S], func:S=>Option[T]):M[T]	= flatMapM(in,  func andThen optionM)
	final def multiMapM[S,T](in:M[S], func:S=>Iterable[T]):M[T]	= flatMapM(in,  func andThen iterableM)
}
