package scparse.ng

import scala.annotation.tailrec

import scutil.base.implicits._
import scutil.lang._
import scutil.lang.tc._

import scparse.ng.ParserResult._

object Parser {
	def pure[S,T](t:T):Parser[S,T]	= success(t)

	def success[S,T](t:T):Parser[S,T]	=
		input => ParserResult.Success(input, t)

	def failure[S]:Parser[S,Nothing]	=
		input => ParserResult.Failure(input.index, List.empty)

	def index[S]:Parser[S,Int]	=
		input => ParserResult.Success(input, input.index)

	def option[S,T](it:Option[T]):Parser[S,T]	=
		it match {
			case None		=> failure
			case Some(t)	=> success(t)
		}

	//------------------------------------------------------------------------------

	def any[S]:Parser[S,S]	=
		input => {
			input.next match {
				case Some((rest, item))	=> Success(rest, item)
				case None				=> LeafFailure(input.index, "end of input")
			}
		}

	def anyCount[S](count:Int):Parser[S,Seq[S]]	=
		input => {
			@tailrec
			def loop(input2:ParserInput[S], accu:Vector[S]):ParserResult[S,Seq[S]]	=
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

	// BETTER use Equal
	def is[S](c:S):Parser[S,S]	=
		any[S] ensure (_ == c) named "specific item"

	// BETTER use Equal
	def isInSet[S](cs:Set[S]):Parser[S,S]	=
		any[S] ensure cs.contains named "item in set"

	def isInRange[S:Ordering](min:S, max:S):Parser[S,S]	=
		any[S] ensure (it => it >= min && it <= max) named "item in range"

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
		any[S].prevents

	//------------------------------------------------------------------------------

	// TODO if we had Foldable, this would work, too
	def choice[S,T](parsers:Iterable[Parser[S,T]]):Parser[S,T]	=
		(parsers foldLeft (Parser.failure[S]:Parser[S,T]))(_ orElse _)

	// NOTE we are applicative, so Iterable.traverse should work, too
	def traverseVector[S,T](parsers:Vector[Parser[S,T]]):Parser[S,Vector[T]]	=
		traverseSeq(parsers) map (_.toVector)

	// NOTE we are applicative, so Iterable.traverse should work, too
	def traverseSeq[S,T](parsers:Seq[Parser[S,T]]):Parser[S,Seq[T]]	=
		parsers match {
			case head +: tail	=> (head nextWith traverseSeq(tail)) { _ +: _}
			case _				=> Parser success Vector.empty
		}

	//------------------------------------------------------------------------------

	// TODO add a MonoidK instance - or even better, got for a full MonadPlus
	implicit def ParserApplicative[S]:Applicative[Parser[S,?]]	=
		new Applicative[Parser[S,?]] {
			override def pure[T](it:T):Parser[S,T]											= Parser success it
			override def ap[T1,T2](its:Parser[S,T1])(func:Parser[S,T1=>T2]):Parser[S,T2]	= its pa func
		}
}

abstract class Parser[S,+T] { self =>
	def parse(input:ParserInput[S]):ParserResult[S,T]

	//------------------------------------------------------------------------------

	def when(pred:Predicate[T]):Parser[S,Option[T]]	=
		self map { it =>
			pred(it) option it
		}

	def ensure(pred:Predicate[T]):Parser[S,T]	=
		self require (_ optionBy pred)

	def requiredFor[U](name:String)(implicit ev:T=>Option[U]):Parser[S,U]	=
		self.required named name

	def required[U](implicit ev:T=>Option[U]):Parser[S,U]	= self require ev

	def requirePartial[U](func:PartialFunction[T,U]):Parser[S,U]	= self require func.lift

	def require[U](func:T=>Option[U]):Parser[S,U]	=
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

	def named(error:String):Parser[S,T]	=
		input => {
			self parse input match {
				case Success(tail, value)	=> Success(tail, value)
				case Failure(index, errors)	=> Failure(index, error :: errors)
			}
		}

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

	def flatten[U](implicit ev:T=>Parser[S,U]):Parser[S,U]	=
		self flatMap ev

	// function effect first
	def ap[U,V](that:Parser[S,U])(implicit ev:T=>(U=>V)):Parser[S,V]	=
		for { a	<- self; b	<- that } yield a(b)

	def next[U](that:Parser[S,U]):Parser[S,(T,U)]	=
		for { a	<- self; b	<- that } yield (a, b)

	def nextWith[U,V](that:Parser[S,U])(combine:(T,U)=>V):Parser[S,V]	=
		for { a	<- self; b	<- that } yield combine(a, b)

	def tag[U](it:U):Parser[S,U]	=
		self map constant(it)

	def void:Parser[S,Unit]	=
		self tag (())

	def left(that:Parser[S,Any]):Parser[S,T]	=
		self next that map { _._1 }

	def right[U](that:Parser[S,U]):Parser[S,U]	=
		self next that map { _._2 }

	//------------------------------------------------------------------------------

	def option:Parser[S,Option[T]]	=
		input => {
			self parse input match {
				case Success(input1, t)	=> Success(input1,	Some(t))
				case Failure(_, _)		=> Success(input,	None)
			}
		}

	def seq:Parser[S,Seq[T]]	= vector

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

	def nes:Parser[S,Nes[T]]	=
		self next self.seq map { case (x, xs) => Nes(x, xs) }

	def times(count:Int):Parser[S,Seq[T]]	=
		// TODO get at the name of self for this
		self timesUpTo count	ensure ( _.size == count) named s"exactly ${count.toString} times"

	def timesInRange(min:Int, max:Int):Parser[S,Seq[T]]	=
		// TODO get at the name of self for this
		self timesUpTo max		ensure (_.size >= min) named s"between ${min.toString}  and ${max.toString} times"

	def timesUpTo(count:Int):Parser[S,Seq[T]]	=
		input => {
			@tailrec
			def loop(input1:ParserInput[S], accu:Seq[T]):ParserResult[S,Seq[T]]	=
					if (accu.size == count)	Success(input1, accu)
					else {
						self parse input1 match {
							case Success(input2, value)	=> loop(input2, accu :+ value)
							case Failure(_, _)			=> Success(input1, accu)
						}
					}
			loop(input, Vector.empty[T])
		}

	def sepSeq(sepa:Parser[S,Any]):Parser[S,Seq[T]]	=
		self sepNes sepa map { _.toVector } orElse (Parser success Vector.empty)

	def sepNes(sepa:Parser[S,Any]):Parser[S,Nes[T]]	=
		self next (sepa right self).seq map { case (x, xs) => Nes(x, xs) }

	//------------------------------------------------------------------------------

	def inside(quote:Parser[S,Any]):Parser[S,T]	=
		quote right self left quote

	def flag:Parser[S,Boolean]	=
		self.option map (_.isDefined)

	//------------------------------------------------------------------------------

	def guards:Parser[S,Unit]	=
		self.prevents.prevents

	def prevents:Parser[S,Unit]	=
		input => {
			self parse input match {
				// TODO get at the name of self for this
				case Success(_, _)	=> LeafFailure(input.index, "prevented")
				case Failure(_, _)	=> Success(input, ())
			}
		}

	//------------------------------------------------------------------------------

	def eatLeft(ws:Parser[S,Any]):Parser[S,T]	=
		ws.option right self

	def eatRight(ws:Parser[S,Any]):Parser[S,T]	=
		self left ws.option

	def finish(ws:Parser[S,Any]):Parser[S,T]	=
		self.eatRight(ws).phrase

	def phrase:Parser[S,T]	=
		self left Parser.end

	//------------------------------------------------------------------------------

	def nest[U,V](mkInput:T=>ParserInput[U], inner:Parser[U,V]):Parser[S,V]	=
		selfInput => {
			self parse selfInput match {
				case Success(selfRemainder, selfValue)	=>
					inner parse mkInput(selfValue) match {
						case Success(innerRemainder, innerValue)	=> Success(selfRemainder, innerValue)
						// TODO deal with the inner index somehow
						case Failure(innerIndex, errors)			=> Failure(selfInput.index, "nest" +: errors)
					}
				case Failure(index, errors)	=> Failure(index, errors)
			}
		}
}
