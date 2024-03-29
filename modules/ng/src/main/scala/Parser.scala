package scparse.ng

import scala.annotation.tailrec
import scala.collection.IterableFactory
import scala.collection.mutable

import scutil.core.implicits.*
import scutil.lang.*
import scutil.lang.tc.*

import scparse.ng.ParserResult.*

object Parser {
	/** this is useful when building a recursive parser */
	def defer[S,T](peer: =>Parser[S,T]):Parser[S,T]	=
		new Parser[S,T] {
			lazy val cached	= peer
			def parse(input:ParserInput[S]):ParserResult[S,T]	= cached parse input
		}

	def pure[S,T](t:T):Parser[S,T]	= success(t)

	def success[S,T](t:T):Parser[S,T]	=
		input => ParserResult.Success(input, t)

	def unit[S]:Parser[S,Unit]	= pure(())

	def failure[S]:Parser[S,Nothing]	=
		input => AnonFailure(input.index)

	def failureWith[S](error:String):Parser[S,Nothing]	=
		input => LeafFailure(input.index, error)

	def index[S]:Parser[S,Int]	=
		input => ParserResult.Success(input, input.index)

	def fromOption[S,T](it:Option[T]):Parser[S,T]	=
		it match {
			case None		=> failure
			case Some(t)	=> success(t)
		}

	def guard[S](condition:Boolean):Parser[S,Unit]	=
		if (condition)	unit
		else			failure

	def prevent[S](condition:Boolean):Parser[S,Unit]	=
		guard(!condition)

	//------------------------------------------------------------------------------

	def any[S]:Parser[S,S]	=
		input => {
			input.next match {
				case Some((rest, item))	=> Success(rest, item)
				case None				=> LeafFailure(input.index, "end of input")
			}
		}

	def take[S](count:Int):Parser[S,IndexedSeq[S]]	=
		input => {
			@tailrec
			def loop(input2:ParserInput[S], accu:Vector[S]):ParserResult[S,IndexedSeq[S]]	=
				if (accu.size == count) {
					Success(input2, accu)
				}
				else {
					input2.next match {
						case Some((tail, item))	=> loop(tail, accu :+ item)
						case _					=> LeafFailure(input2.index, "end of input")
					}
				}
			loop(input, Vector.empty)
		}

	def skip[S](count:Int):Parser[S,Unit]	=
		input => {
			@tailrec
			def loop(input2:ParserInput[S], accu:Int):ParserResult[S,Unit]	=
				if (accu == count) {
					Success(input2, ())
				}
				else {
					input2.next match {
						case Some((tail, _))	=> loop(tail, accu + 1)
						case _					=> LeafFailure(input2.index, "end of input")
					}
				}
			loop(input, 0)
		}

	// TODO can be optimized quite a bit
	def remainder[S]:Parser[S,IndexedSeq[S]]	=
		any.vector

	def satisfy[S](pred:Predicate[S]):Parser[S,S]		=
		any filter pred

	def inSet[S](cs:Set[S]):Parser[S,S]	=
		satisfy(cs.contains) named "item in set"

	def notInSet[S](cs:Set[S]):Parser[S,S]	=
		satisfy(!cs.contains(_:S)) named "item not in set"

	def inRange[S:Ordering](min:S, max:S):Parser[S,S]	=
		satisfy[S](it => it >= min && it <= max) named "item in range"

	def notInRange[S:Ordering](min:S, max:S):Parser[S,S]	=
		satisfy[S](it => it < min || it > max) named "item not in range"

	// BETTER use Equal
	def is[S](c:S):Parser[S,S]	=
		satisfy[S](_ == c) named "specific item"

	def isSeq[S](cs:Seq[S]):Parser[S,Seq[S]]	=
		input => {
			@tailrec
			def loop(input2:ParserInput[S], look:Seq[S]):ParserResult[S,Seq[S]]	=
					look match {
						case lookHead +: lookTail	=>
							 input2.next match {
								case Some((tail, item)) if item == lookHead	=> loop(tail, lookTail)
								case _										=> LeafFailure(input2.index, "end of input")
							}
						case _	=>
							Success(input2, cs)
					}
			loop(input, cs)
		}

	def end[S]:Parser[S,Unit]	=
		any[S].not

	// TODO if we had Foldable, this would work, too
	def choice[S,T](parsers:Iterable[Parser[S,T]]):Parser[S,T]	=
		(parsers foldLeft (Parser.failure[S]:Parser[S,T]))(_ orElse _)

	def choiceOf[S,T](parsers:Parser[S,T]*):Parser[S,T]	=
		choice(parsers)

	//------------------------------------------------------------------------------

	// NOTE we are applicative, so Iterable.traverse should work, too
	def traverseVector[S,T](parsers:Vector[Parser[S,T]]):Parser[S,Vector[T]]	=
		traverseSeq(parsers) map (_.toVector)

	def traverseList[S,T](parsers:List[Parser[S,T]]):Parser[S,List[T]]	=
		traverseSeq(parsers) map (_.toList)

	// NOTE we are applicative, so Iterable.traverse should work, too
	def traverseSeq[S,T](parsers:Seq[Parser[S,T]]):Parser[S,Seq[T]]	=
		parsers match {
			case head +: tail	=> (head nextWith traverseSeq(tail)) { _ +: _}
			case _				=> Parser success Vector.empty
		}

	def traverseNes[S,T](parsers:Nes[Parser[S,T]]):Parser[S,Nes[T]]	=
		traverseSeq(parsers.toVector) map { it => Nes fromSeq it getOrError "nes parser always expected to create at least 1 result" }

	//------------------------------------------------------------------------------

	// TODO add a MonoidK instance - or even better, got for a full MonadPlus
	given [S]:Applicative[Parser[S,_]]	=
		new Applicative[Parser[S,_]] {
			override def pure[T](it:T):Parser[S,T]											= Parser success it
			override def ap[T1,T2](func:Parser[S,T1=>T2])(its:Parser[S,T1]):Parser[S,T2]	= func ap its
		}
}

abstract class Parser[S,+T] { self =>
	def parse(input:ParserInput[S]):ParserResult[S,T]

	//------------------------------------------------------------------------------

	def filter(pred:Predicate[T]):Parser[S,T]	=
		self mapFilter (_ optionBy pred)

	/** ensures we parsed some specific input value */
	def parsed[TT>:T](value:TT):Parser[S,Unit]	=
		self.filter(_ == value).void

	def mapFilter[U](func:T=>Option[U]):Parser[S,U]	=
		input => {
			self parse input match {
				case Success(tail, value)	=>
					func(value) match {
						case None			=> AnonFailure(input.index)
						case Some(value2)	=> Success(tail, value2)
					}
				case Failure(index, errors)	=> Failure(index, errors)
			}
		}

	def flattenOption[U](using ev:T <:< Option[U]):Parser[S,U]	= self mapFilter ev

	def flattenOptionNamed[U](name:String)(using ev:T <:< Option[U]):Parser[S,U]	=
		self.flattenOption named name

	def collect[U](func:PartialFunction[T,U]):Parser[S,U]	= self mapFilter func.lift

	//------------------------------------------------------------------------------

	def orElse[TT>:T](that:Parser[S,TT]):Parser[S,TT]	=
		input => {
			(self parse input, that parse input) match {
				case (f1@	Failure(i1, _),		f2@	Failure(i2, _))		=>
					// TODO should we return both errors if they are at the same position?
					if (i1 >= i2)	f1
					else			f2
				case (s1@	Success(i1, v1),		Failure(_, _))		=> s1
				case (		Failure(_, _),		s2@	Success(i2, v2))	=> s2
				case (s1@	Success(i1, v1),		Success(_, _))		=> s1
			}
		}

	def either[U](that:Parser[S,U]):Parser[S,Either[T,U]]	=
		(self map Either.left[T,U])	orElse
		(that map Either.right[T,U])

	//------------------------------------------------------------------------------

	def map[U](func:T=>U):Parser[S,U]	=
		input => {
			self parse input match {
				case Success(index, t)	=> Success(index, func(t))
				case Failure(index, e)	=> Failure(index, e)
			}
		}

	def flatMap[U](func:T=>Parser[S,U]):Parser[S,U]	=
		input => {
			self parse input match {
				case Success(input1, t)		=> func(t) parse input1
				case Failure(index, errors)	=> Failure(index, errors)
			}
		}

	def flatten[U](using ev:T <:< Parser[S,U]):Parser[S,U]	=
		self flatMap ev

	def ap[U,V](that:Parser[S,U])(using ev:T <:< (U=>V)):Parser[S,V]	=
		for { a	<- self; b	<- that } yield a(b)

	def product[U](that:Parser[S,U]):Parser[S,(T,U)]	=
		for { a	<- self; b	<- that } yield (a, b)

	def map2[U,V](that:Parser[S,U])(combine:(T,U)=>V):Parser[S,V]	=
		for { a	<- self; b	<- that } yield combine(a, b)

	def next[U](that:Parser[S,U]):Parser[S,(T,U)]	=
		this product that

	//@deprecated("use map2", "xxx")
	def nextWith[U,V](that:Parser[S,U])(combine:(T,U)=>V):Parser[S,V]	=
		this.map2(that)(combine)

	//@deprecated("use as", "xxx")
	def tag[U](it:U):Parser[S,U]	=
		self map constant(it)

	def void:Parser[S,Unit]	=
		self tag (())

	//@deprecated("use productL", "xxx")
	def left(that:Parser[S,Any]):Parser[S,T]	=
		self next that map { _._1 }

	//@deprecated("use productR", "xxx")
	def right[U](that:Parser[S,U]):Parser[S,U]	=
		self next that map { _._2 }

	//------------------------------------------------------------------------------

	def flag:Parser[S,Boolean]	=
		self.option map (_.isDefined)

	def optionBy(pred:Predicate[T]):Parser[S,Option[T]]	=
		self map { it =>
			pred(it) option it
		}

	def option:Parser[S,Option[T]]	=
		input => {
			self parse input match {
				case Success(input1, t)	=> Success(input1,	Some(t))
				case Failure(_, _)		=> Success(input,	None)
			}
		}

	def seq:Parser[S,Seq[T]]	= vector

	def indexedSeq:Parser[S,IndexedSeq[T]]	= vector

	def vector:Parser[S,Vector[T]]	=
		input => {
			@tailrec
			def loop(input1:ParserInput[S], accu:Vector[T]):ParserResult[S,Vector[T]]	=
					self parse input1 match {
						case Success(input2, t)	=> loop(input2, accu :+ t)
						case Failure(_, _)		=> Success(input1, accu)
					}
			loop(input, Vector.empty[T])
		}

	def list:Parser[S,List[T]]	= vector map (_.toList)

	def nes:Parser[S,Nes[T]]	=
		self next self.seq map { case (x, xs) => Nes(x, xs) }

	def times(count:Int):Parser[S,IndexedSeq[T]]	=
		// TODO get at the name of self for this
		self timesUpTo count	filter ( _.size == count) named s"exactly ${count.toString} times"

	def timesInRange(min:Int, max:Int):Parser[S,IndexedSeq[T]]	=
		// TODO get at the name of self for this
		self timesUpTo max		filter (_.size >= min) named s"between ${min.toString}  and ${max.toString} times"

	def timesUpTo(count:Int):Parser[S,IndexedSeq[T]]	=
		input => {
			@tailrec
			def loop(input1:ParserInput[S], accu:IndexedSeq[T]):ParserResult[S,IndexedSeq[T]]	=
				if (accu.size == count)	Success(input1, accu)
				else {
					self parse input1 match {
						case Success(input2, value)	=> loop(input2, accu :+ value)
						case Failure(_, _)			=> Success(input1, accu)
					}
				}
			loop(input, Vector.empty[T])
		}

	def vectorSepBy(sepa:Parser[S,Any]):Parser[S,Vector[T]]	=
		self nesSepBy sepa map { _.toVector } orElse (Parser success Vector.empty)

	def indexedSeqSepBy(sepa:Parser[S,Any]):Parser[S,IndexedSeq[T]]	=
		vectorSepBy(sepa)

	def seqSepBy(sepa:Parser[S,Any]):Parser[S,Seq[T]]	=
		vectorSepBy(sepa)

	def listSepBy(sepa:Parser[S,Any]):Parser[S,List[T]]	=
		vectorSepBy(sepa) map (_.toList)

	def nesSepBy(sepa:Parser[S,Any]):Parser[S,Nes[T]]	=
		//self next (sepa right self).seq map { case (x, xs) => Nes(x, xs) }
		self nesWith (sepa right self).seq

	// TODO i need this for Vector (or IndexedSeq?)
	def cons[TT>:T](that: =>Parser[S,List[TT]]):Parser[S,List[TT]]	=
		(this nextWith that)(_ :: _)

	def nesWith[TT>:T](that: =>Parser[S,Seq[TT]]):Parser[S,Nes[TT]]	=
		(this nextWith that)(Nes.apply)

	//------------------------------------------------------------------------------

	def seqTo[CC[_],U](factory:IterableFactory[CC])(using ev:T <:< IterableOnce[U]):Parser[S,CC[U]]	=
		map { it =>
			factory from it
		}

	def nesTo[CC[_],U](factory:IterableFactory[CC])(using ev:T <:< Nes[U]):Parser[S,CC[U]]	=
		map { it =>
			factory from ev(it).toSeq
		}

	//------------------------------------------------------------------------------

	def chainLeft[U>:T](op:Parser[S,(U,U)=>U]):Parser[S,U]	=
		for {
			first	<-	this
			apps	<-	(op next this).vector
		}
		yield {
			(apps foldLeft (first:U)) { (cur, app) =>
				val (op, nxt)	= app
				op(cur, nxt)
			}
		}

	// TODO does this do the same thing as the one in oldschool?
	def chainRight[U>:T](op:Parser[S,(U,U)=>U]):Parser[S,U]	=
		for {
			apps	<-	(this next op).vector
			last	<-	this
		}
		yield {
			(apps foldRight (last:U)) { (app, cur) =>
				val (prv, op)	= app
				op(prv, cur)
			}
		}

	//------------------------------------------------------------------------------

	def within(quote:Parser[S,Any]):Parser[S,T]	=
		quote right self left quote

	//------------------------------------------------------------------------------

	def not:Parser[S,Unit]	=
		input => {
			self parse input match {
				// TODO get at the name of self for this
				case Success(_, _)	=> LeafFailure(input.index, "prevented")
				case Failure(_, _)	=> Success(input, ())
			}
		}

	//------------------------------------------------------------------------------

	/** eatLeft and finishRight combine into a tokenizer, as eatRight and finishLeft do */
	def eatLeft(ws:Parser[S,Any]):Parser[S,T]	=
		ws.option right self

	/** eatLeft and finishRight combine into a tokenizer, as eatRight and finishLeft do */
	def eatRight(ws:Parser[S,Any]):Parser[S,T]	=
		self left ws.option

	/** eatLeft and finishRight combine into a tokenizer, as eatRight and finishLeft do */
	def finishRight(ws:Parser[S,Any]):Parser[S,T]	=
		self.eatRight(ws).phrase

	/** eatLeft and finishRight combine into a tokenizer, as eatRight and finishLeft do */
	def finishLeft(ws:Parser[S,Any]):Parser[S,T]	=
		self.eatLeft(ws).phrase

	/** expects a full parse, fails if anything is left in the source */
	def phrase:Parser[S,T]	=
		self left Parser.end

	//------------------------------------------------------------------------------

	def named(error:String):Parser[S,T]	=
		input => {
			self parse input match {
				case Success(tail, value)	=> Success(tail, value)
				case Failure(index, errors)	=> Failure(index, error :: errors)
			}
		}

	def padTo(length:Int):Parser[S,T]	=
		for {
			before		<-	Parser.index
			value		<-	this
			after		<-	Parser.index
			_			<-	{
								val taken		= after - before
								if (taken <= length)	Parser.skip(length - taken)
								else					Parser.failureWith(s"expected to take at most ${length} items, but took ${taken} items")
							}
		}
		yield value

	def nest[U,V](mkInput:T=>ParserInput[U], inner:Parser[U,V]):Parser[S,V]	=
		selfInput => {
			self parse selfInput match {
				case Success(selfRemainder, selfValue)	=>
					val innerInput	= mkInput(selfValue)
					inner parse innerInput match {
						case Success(innerRemainder, innerValue)	=> Success(selfRemainder, innerValue)
						// TODO deal with the inner index somehow
						case Failure(innerIndex, errors)			=> Failure(selfInput.index, "nest" +: errors)
					}
				case Failure(index, errors)	=> Failure(index, errors)
			}
		}

	def scanner:Parser[S,IndexedSeq[T]]	=
		(input:ParserInput[S]) => {
			val out	= mutable.ArrayBuffer.empty[T]
			@tailrec
			def loop(ss:ParserInput[S]):ParserResult[S,IndexedSeq[T]]	= {
				self parse ss match {
					case Success(rest, value)	=>
						out	+= value
						loop(rest)
					case Failure(_, _) =>
						// TODO ugly, but without match we don't get tailrec
						val more	=
							ss.next.cata(
								None,
								{ case (rest,_) => Some(rest) }
							)
						more match {
							case Some(x)	=> loop(x)
							case None		=> Success(ss, out.toVector)
						}
				}
			}
			loop(input)
		}

	//------------------------------------------------------------------------------

	def withFilter[TT>:T](predicate:TT=>Boolean)	= new GenWithFilter[TT](this, predicate)

	class GenWithFilter[TT>:T](peer:Parser[S,TT], predicate:TT=>Boolean) {
		def map[U](func:TT=>U):Parser[S,U]						= peer filter predicate map func
		def flatMap[U](func:TT=>Parser[S,U]):Parser[S,U]		= peer filter predicate flatMap	func
		def withFilter(further:TT=>Boolean):GenWithFilter[TT]	= new GenWithFilter(peer, x => predicate(x) && further(x))
	}
}
