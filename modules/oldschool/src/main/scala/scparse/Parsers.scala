package scparse.oldschool

import scala.language.implicitConversions
import scala.collection.mutable

import scutil.lang._

class Parsers[M[+_]](implicit val base:Base[M]) { outer =>
	import base._

	type Result[+C,+T]		= M[Item[C,T]]
	type Item[+C,+T]		= (Source[C],T)

	// lift simple values to parsers
	implicit def liftValue[C](c:C):Parser[C,C]			= accept(c)
	implicit def liftSeq[C](cs:Seq[C]):Parser[C,Seq[C]]	= literal(cs)

	//------------------------------------------------------------------------------
	//## factory

	def Parser[C,T](func:Source[C]=>Result[C,T]):Parser[C,T]	=
		new Parser[C,T] {
			def parse(s:Source[C]):Result[C,T]	= func(s)
		}

	// Pointed, aka unit aka return aka pure
	def success[C,T](t:T):Parser[C,T]	=
		it => unitM((it, t))

	// MonadZero, aka zero
	def failure[C]:Parser[C,Nothing]	=
		_ => zeroM

	// base case
	def any[C]:Parser[C,C]	=
		_.cata(
			zeroM,
			(x,y) => unitM((x,y))
		)

	// MonadZero (Alternative?)
	def filter[C,T](sub: =>Parser[C,T], func:T=>Boolean):Parser[C,T] =
		// Parser(s => sub(s) filter { rt => func(rt._2) })
		s => {
			filterM(
				sub parse s,
				(it:Item[C,T]) => func(it._2)
			)
		}

	def filterNot[C,T](sub: =>Parser[C,T], func:T=>Boolean):Parser[C,T] =
		filter(sub, !func(_:T))

	def collapseMap[C,T,U](sub: =>Parser[C,T], func:T=>Option[U]):Parser[C,U]	=
		// Parser(s => sub(s) flatMap { rt => func(rt._2) map { (rt._1, _) } })
		(s:Source[C]) => {
			collapseMapM(
				sub parse s,
				(it:Item[C,T]) => func(it._2) map {(it._1, _)}
			)
		}

	// NOTE this just takes the first item for PEG parsers
	def multiMap[C,T,U](sub: =>Parser[C,T], func:T=>Iterable[U]):Parser[C,U]	=
		//Parser(s => sub(s) flatMap { rt => func(rt._2) map { (rt._1, _) } })
		(s:Source[C]) => {
			multiMapM(
				sub parse s,
				(it:Item[C,T]) => func(it._2) map {(it._1, _)}
			)
		}

	// Functor, aka fmap
	def map[C,T,U](sub: =>Parser[C,T], func:T=>U):Parser[C,U]	=
		//Parser(s => sub(s) map { rt => (rt._1, func(rt._2)) })
		(s:Source[C]) => {
			mapM(
				sub parse s,
				(it:Item[C,T]) => (it._1, func(it._2))
			)
		}

	// Monad, aka bind aka >==
	def flatMap[C,T,U](sub: =>Parser[C,T], func:T=>Parser[C,U]):Parser[C,U]	=
		// Parser(s => sub(s) flatMap { rt => func(rt._2)(rt._1) })
		(s:Source[C]) => {
			flatMapM(
				sub parse s,
				(it:Item[C,T]) => func(it._2) parse (it._1)
			)
		}

	//  Applicative, aka <*> aka ap
	def applicate[C,S,T](mapping: =>Parser[C,S=>T], value: =>Parser[C,S]):Parser[C,T] =
		//Parser(s => mapping(s) flatMap { rf => value(rf._1) map { st => (st._1, rf._2(st._2)) } })
		// TODO use applicM	if possible
		(s:Source[C]) => {
			flatMapM(
				mapping parse s,
				(rf:Item[C,S=>T]) => {
					mapM(
						value parse rf._1,
						(st:Item[C,S]) => (st._1, rf._2(st._2))
					)
				}
			)
		}

	// MonadPlus (Alternative?)
	def alternate[C,T,U>:T](first: =>Parser[C,T], second: =>Parser[C,U]):Parser[C,U]	=
		s => alternateM(first parse s, second parse s)

	/** prefer first parser over second */
	def prefer[C,T,U>:T](first: =>Parser[C,T], second: =>Parser[C,U]):Parser[C,U]	=
		s => preferM(first parse s, second parse s)

	/** swaps success and failure, never consumes any input */
	def not[C,T](sub: =>Parser[C,T]):Parser[C,Unit] =
		s => {
			if (isEmptyM(sub parse s))	unitM((s, ()))
			else						zeroM
		}

	/** checks, but never consumes any input */
	def ahead[C,T](sub: =>Parser[C,T]):Parser[C,Unit] =
		s => {
			if (nonEmptyM(sub parse s))	unitM((s, ()))
			else						zeroM
		}

	//------------------------------------------------------------------------------

	/** fails if anything is left in the source */
	def end[C]:Parser[C,Unit]	= not(any)

	/** expects a full parse, fails if anything is left in the source */
	def phrase[C,T](sub: =>Parser[C,T]):Parser[C,T]	=
		left(sub, end)

	/** scalaesque filterMap using a PartialFunction */
	def collect[C,T,U](sub: =>Parser[C,T], func:PartialFunction[T,U]):Parser[C,U]	=
		collapseMap(sub, func.lift)

	/** returns a fixed value on success */
	def tag[C,T,U](sub: =>Parser[C,T], value: =>U):Parser[C,U]	=
		map(sub, (_:T) => value)

	/** sequences two parsers producing a Pair */
	def next[C,T,U](first: =>Parser[C,T], second: =>Parser[C,U]):Parser[C,(T,U)]	=
		// for { p1 <- parser1; p2 <- parser2 } yield Pair(p1, p2)
		// applicate(applicate(success((t:T) => (u:U) => Pair(t,u)), parser1), parser2)
		flatMap(first, (value1:T) =>
			map(second, (value2:U) =>
			  (value1, value2)
			)
		)

	/** sequences a normal and a List parser producing a List */
	def cons[C,T,U>:T](first: =>Parser[C,T], second: =>Parser[C,List[U]]):Parser[C,List[U]]	=
		// for { p1 <- parser1; p2 <- parser2 } yield p1 :: p2
		// applicate(applicate(success((t:T) => (uu:List[U]) => t :: uu), parser1), parser2)
		flatMap(first, (value1:T) =>
			map(second, (value2:List[U]) =>
				value1 :: value2
			)
		)

	/** sequences two List parsers into a single List parser */
	def conses[C,T,U>:T](first: =>Parser[C,List[T]], second: =>Parser[C,List[U]]):Parser[C,List[U]]	=
		flatMap(first, (value1:List[T]) =>
			map(second, (value2:List[U]) =>
				value1 ::: value2
			)
		)

	/** optional */
	def option[C,T](sub: =>Parser[C,T]):Parser[C,Option[T]]	=
		alternate(map(sub, Some.apply[T]), success(None))

	/** repeat at least 1 */
	def repeat1[C,T](sub: =>Parser[C,T]):Parser[C,List[T]]	=
		cons(sub, repeat(sub))

	/** repeat zero or more */
	def repeat[C,T](sub: =>Parser[C,T]):Parser[C,List[T]]	=
		alternate(repeat1(sub), success(Nil))

	/** repeat n times */
	def repeatN[C,T](sub: =>Parser[C,T], n:Int):Parser[C,List[T]] =
		if (n == 0)	success(Nil)
		else		cons(sub, repeatN(sub, n-1))

	def repeatSeparated1[C,T,U](item: =>Parser[C,T], separator: =>Parser[C,U]):Parser[C,List[T]]	=
		cons(item, repeat(right(separator, item)))

	def repeatSeparated[C,T,U](item: =>Parser[C,T], separator: =>Parser[C,U]):Parser[C,List[T]]	=
		alternate(repeatSeparated1(item, separator), success(Nil))

	/** sequences, but returns only the first */
	def left[C,T,U](value: =>Parser[C,T], ignored: =>Parser[C,U]):Parser[C,T] =
		map(next(value, ignored), (it:(T,U)) => it._1)

	/** sequences, but returns only the second */
	def right[C,T,U](ignored: =>Parser[C,T], value: =>Parser[C,U]):Parser[C,U] =
		map(next(ignored, value), (it:(T,U)) => it._2)

	def chainLeft[C,T,U>:T](value: =>Parser[C,T], operator: =>Parser[C,(U,U)=>U]):Parser[C,U]	= {
		def rest(value1:U):Parser[C,U]	= {
			alternate(
				flatMap(operator, (valueOp:((U,U)=>U)) =>
					flatMap(value, (value2:T) =>
						rest(valueOp(value1,value2))
					)
				),
				success(value1)
			)
		}
		flatMap(value, rest _)
	}

	def chainRight[C,T,U>:T](value: =>Parser[C,T], operator: =>Parser[C,(U,U)=>U]):Parser[C,U]	= {
		flatMap(value, (value1:T) =>
			alternate(
				flatMap(operator, (valueOp:((U,U)=>U)) =>
					map(chainRight(value, operator), (value2:U) =>
						valueOp(value1, value2)
					)
				),
				success(value1)
			)
		)
	}

	//------------------------------------------------------------------------------

	def accept[C](pattern:C):Parser[C,C]					= satisfy	{ _ == pattern }
	def literal[C](pattern:Seq[C]):Parser[C,Seq[C]]			= take(pattern.size) filter pattern.sameElements
	def literalList[C](pattern:List[C]):Parser[C,List[C]]	= sequenceList(pattern map accept)

	def satisfy[C](pred:C=>Boolean):Parser[C,C]	= filter(any[C], pred)
	def anyInclude[C](cs:C*):Parser[C,C]		= satisfy { c => cs exists { c == _ } }
	def anyExclude[C](cs:C*):Parser[C,C]		= satisfy { c => cs forall { c != _ } }

	//------------------------------------------------------------------------------

	/*
	msum :: MonadPlus m => [m a] -> m a
	msum = foldr mplus mzero
	*/
	def alternateMultiple[C,T](subs: =>Seq[Parser[C,T]]):Parser[C,T]	=
		subs.foldLeft(failure:Parser[C,T])(alternate(_,_))

	// NOTE this is the same as alternateMultiple for PEG parsers
	def preferMultiple[C,T](subs: =>Seq[Parser[C,T]]):Parser[C,T]	=
		subs.foldLeft(failure:Parser[C,T])(prefer(_,_))

	def sequenceList[C,T](subs: =>List[Parser[C,T]]):Parser[C,List[T]]	=
		subs match {
			case head :: tail	=> cons(head, sequenceList(tail))
			case Nil			=> success(Nil)
		}

	//------------------------------------------------------------------------------
	//## extras

	/** ignore the output, just check for a match */
	def void[C,T](sub: =>Parser[C,T]):Parser[C,Unit]	= tag(sub, ())

	/** gives a fixed number of input tokens  */
	def take[C,T](count:Int):Parser[C,Seq[C]]	= {
		def impl(s:Source[C]):Result[C,Seq[C]]	= {
			val	out	= mutable.ArrayBuffer.empty[C]
			var in	= s
			var i	= 0
			while (true) {
				if (i == count)	return unitM((in, out.toVector))
				val exit	=
					in.cata(
						true,
						(more,item)	=> { out += item; in = more; i += 1; false }
					)
				if (exit)	return zeroM
			}
			nothing
		}
		Parser(impl)
	}

	/** gives the complete rest of the input */
	def remainder[C,T]:Parser[C,Seq[C]]	= {
		def impl(s:Source[C]):Result[C,Seq[C]]	= {
			val	out	= mutable.ArrayBuffer.empty[C]
			var in	= s
			while (true) {
				val exit	=
					in.cata(
						true,
						(more,item)	=> { out += item; in = more; false }
					)
				if (exit)	return unitM((in, out.toVector))
			}
			nothing
		}
		Parser(impl)
	}

	/** parse into a source, then parse inside that source */
	def inside[C,D,DS,T](a: =>Parser[C,DS], b: =>Parser[D,T])(implicit ev:DS <:< Source[D]):Parser[C,T] =
		nest(ev, a, b)

	/** parse into a source, then parse inside that source */
	def nest[C,D,DS,T](mkSource:DS=>Source[D], a: =>Parser[C,DS], b: =>Parser[D,T]):Parser[C,T] = {
		def impl(sc:Source[C]):Result[C,T]	= {
			val aRef	= a
			val bRef	= b
			val dss:Result[C,DS]	= aRef parse sc
			val ts:Result[C,T]		= flatMapM(dss, (it:Item[C,DS]) => {
				mapM(bRef parse mkSource(it._2), { (jt:Item[D,T])	=>
					(it._1, jt._2)
				})
			})
			ts
		}
		Parser(impl)
	}

	//------------------------------------------------------------------------------
	//## performance optimization

	def fastRepeat[C,T](sub: =>Parser[C,T]):Parser[C,Seq[T]]	= {
		type Part	= Item[C,T]
		type Step	= Item[C,Seq[T]]

		def impl(s:Source[C]):Result[C,Seq[T]]	= {
			val subRef	= sub
			var current:M[Step]	= unitM((s, Vector.empty))
			var out:M[Step]		= current
			while (true) {
				val step:Step=>M[Step]	= { case (src,it) =>
					val x:M[Part]	= subRef parse src
					val app:Part=>Step	= { case (s1,t1) =>
						(s1, it :+ t1)
					}
					mapM(x, app)
				}
				current	= flatMapM(current, step)
				// TODO check this is OK for Option,
				// it should prefer the longer, right?
				out		= alternateM(current, out)
				if (isEmptyM(current))	return out
			}
			nothing
		}

		Parser(impl)
	}

	def fastRepeat1[C,T](sub: =>Parser[C,T]):Parser[C,Seq[T]]	= {
		type Part	= Item[C,T]
		type Step	= Item[C,Seq[T]]

		def impl(s:Source[C]):Result[C,Seq[T]]	= {
			val subRef	= sub
			var out:M[Step]		= zeroM
			var current:M[Step]	= unitM((s, Vector.empty))
			while (true) {
				val step:Step=>M[Step]	= { case (src,it) =>
					val x:M[Part]	= subRef parse src
					val app:Part=>Step	= { case (s1,t1) =>
						(s1, it :+ t1)
					}
					mapM(x, app)
				}
				current	= flatMapM(current, step)
				// TODO check this is OK for Option,
				// it should prefer the longer, right?
				out		= alternateM(current, out)
				if (isEmptyM(current))	return out
			}
			nothing
		}

		Parser(impl)
	}

	//------------------------------------------------------------------------------
	//## debugging

	def debug[C,T](sub: =>Parser[C,T], before:Effect[Source[C]], after:Effect[Result[C,T]]):Parser[C,T]	=
		s => {
			before(s)
			val r = sub parse s
			after(r)
			r
		}

	//------------------------------------------------------------------------------
	//## parser itself

	trait Parser[C,+T] {
		//## entry

		def parsePhrase(s:Source[C]):M[T]	=
			mapM(
				this.phrase parse s,
				(it:Item[C,T]) => it._2
			)

		//## scala

		def parse(s:Source[C]):Result[C,T]

		def map[U](func:T=>U):Parser[C,U]	= outer.map(this, func)
		def tag[U](value: =>U):Parser[C,U]	= outer.tag(this, value)
		def void:Parser[C,Unit]				= outer.tag(this, ())

		def flatMap[U](func:T=>Parser[C,U]):Parser[C,U]	= outer.flatMap(this, func)

		def filter(func:T=>Boolean):Parser[C,T]		= outer.filter(this, func)
		def withFilter(predicate:T=>Boolean)	= new GenWithFilter[T](this, predicate)
		class GenWithFilter[+A](self:Parser[C,A], predicate:A=>Boolean) {
			def map[B](func:A=>B):Parser[C,B]					= self filter predicate map		func
			def flatMap[B](func:A=>Parser[C,B]):Parser[C,B]		= self filter predicate flatMap	func
			def withFilter(further:A=>Boolean):GenWithFilter[A]	= new GenWithFilter[A](self, x => predicate(x) && further(x))
		}

		// TODO orElse?
		def alternate[U>:T](that: =>Parser[C,U]):Parser[C,U]	= outer .alternate		(this, that)
		// TODO orElse?
		def prefer  [U>:T](that: =>Parser[C,U]):Parser[C,U]		= outer .prefer			(this, that)

		def collect [U](func:PartialFunction[T,U]):Parser[C,U]	= outer .collect		(this, func)
		def collapseMap[U](func:T=>Option[U]):Parser[C,U]		= outer .collapseMap	(this, func)
		def ap[U,V](that: =>Parser[C,U])
				(implicit witness:T <:< (U=>V)):Parser[C,V]		= outer .applicate	(outer.map(this, witness), that)

		def next [U](that: =>Parser[C,U]):Parser[C,(T,U)]		= outer .next		(this, that)
		def left[U](that: =>Parser[C,U]):Parser[C,T] 			= outer .left		(this, that)
		def right[U](that: =>Parser[C,U]):Parser[C,U]			= outer .right		(this, that)

		def cons[TT>:T](that: =>Parser[C,List[TT]]):Parser[C,List[TT]]	=
			outer .cons		(this, that)

		def conses[U>:T](that: =>Parser[C,List[U]])(implicit witness:T<:<List[U]):Parser[C,List[U]]	=
			outer .conses	(outer.map(this, witness), that)

		def not :Parser[C,Unit]			= outer not		(this)

		def option :Parser[C,Option[T]]	= outer option	(this)
		def repeat :Parser[C,List[T]]	= outer repeat	(this)
		def repeat1 :Parser[C,List[T]]	= outer repeat1	(this)

		def repeatN(count:Int):Parser[C,List[T]]							= outer .repeatN	(this, count)
		def repeatSeparated[U](separator: =>Parser[C,U]):Parser[C,List[T]]	= outer .repeatSeparated		(this, separator)
		def repeatSeparated1[U](separator: =>Parser[C,U]):Parser[C,List[T]]	= outer .repeatSeparated1	(this, separator)

		def chainLeft[U>:T](operator: =>Parser[C,(U,U)=>U]):Parser[C,U]		= outer .chainLeft			(this, operator)
		def chainRight[U>:T](operator: =>Parser[C,(U,U)=>U]):Parser[C,U]	= outer .chainRight			(this, operator)

		def phrase :Parser[C,T]											= outer phrase	(this)

		def nest[D,U](mkSource:T=>Source[D], inner:Parser[D,U]):Parser[C,U]	=
			outer.nest(mkSource, this, inner)

		//def inside[C,D,DS,T](a: =>Parser[C,DS], b: =>Parser[D,T])(implicit ev:DS <:< Source[D]):Parser[C,T] = {

		/*
		//## dsl

		def |  [U>:T](that: =>Parser[C,U]):Parser[C,U]				= outer alternate	(this, that)
		def /  [U>:T](that: =>Parser[C,U]):Parser[C,U]				= outer prefer		(this, that)
		def ??    (func:T=>Boolean):Parser[C,T]						= outer filter		(this, func)
		def ^^ [U](func:T=>U):Parser[C,U]							= outer map			(this, func)
		def ^^^[U](value: =>U):Parser[C,U]							= outer tag			(this, value)
		def ^? [U](func:PartialFunction[T,U]):Parser[C,U]			= outer collect		(this, func)
		def ^^?[U](func:T=>Option[U]):Parser[C,U]					= outer filterMap	(this, func)
		def ==>[U](func:T=>Parser[C,U]):Parser[C,U]					= outer flatMap		(this, func)
		def <*>[U,V](that: =>Parser[C,U])
				(implicit witness:T <:< (U=>V)):Parser[C,V]			= outer applicate	(outer map (this, witness), that)
		def <**>[U](that: =>Parser[C,T=>U]):Parser[C,U]				= outer applicate	(that, this)

		def ~ [U](that: =>Parser[C,U]):Parser[C,(T,U)]				= outer next	(this, that)
		def <~[U](that: =>Parser[C,U]):Parser[C,T] 					= outer left		(this, that)
		def ~>[U](that: =>Parser[C,U]):Parser[C,U]					= outer right		(this, that)
		// TODO should not be eager, see https://issues.scala-lang.org/browse/SI-1980
		def ::[U](that: Parser[C,U])
				(implicit witness:T<:<List[U]):Parser[C,List[U]]	= outer cons		(that, outer map (this, witness))
		def :::[U](that: Parser[C,List[U]])
				(implicit witness:T<:<List[U]):Parser[C,List[U]]	= outer conses		(that, outer map (this, witness))

		def unary_! :Parser[C,Unit]									= outer not		(this)

		def ? :Parser[C,Option[T]]									= outer option	(this)
		def * :Parser[C,List[T]]									= outer repeat	(this)
		def + :Parser[C,List[T]]									= outer repeat1	(this)

		def *#(count:Int):Parser[C,List[T]]							= outer repeatN	(this, count)
		def *%[U](separator: =>Parser[C,U]):Parser[C,List[T]]		= outer repeatSeparated		(this, separator)
		def +%[U](separator: =>Parser[C,U]):Parser[C,List[T]]		= outer repeatSeparated1	(this, separator)

		def <<[U>:T](operator: =>Parser[C,(U,U)=>U]):Parser[C,U]	= outer chainLeft			(this, operator)
		def >>[U>:T](operator: =>Parser[C,(U,U)=>U]):Parser[C,U]	= outer chainRight			(this, operator)

		def $ :Parser[C,T]											= outer phrase	(this)
		*/
	}
}
