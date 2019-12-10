package scparse.ng

import scala.annotation.tailrec

import scutil.base.implicits._
import scutil.lang._

import scparse.ng.ParseResult._

object Parser {
	def success[S,T](t:T):Parser[S,T]	=
			input => ParseResult.Success(input, t)

	def failure[S]:Parser[S,Nothing]	=
			input => ParseResult.Failure(input.index, List.empty)

	def index[S]:Parser[S,Int]	=
			input => ParseResult.Success(input, input.index)

	def option[S,T](it:Option[T]):Parser[S,T]	=
			it match {
				case None		=> failure
				case Some(t)	=> success(t)
			}

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
				def loop(input2:ParserInput[S], accu:Vector[S]):ParseResult[S,Seq[S]]	=
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

	def traverseVector[S,T](parsers:Vector[Parser[S,T]]):Parser[S,Vector[T]]	=
			traverseSeq(parsers) map (_.toVector)

	// TODO generalize, we are applicative after all
	def traverseSeq[S,T](parsers:Seq[Parser[S,T]]):Parser[S,Seq[T]]	=
			parsers match {
				case head +: tail	=> (head nextWith traverseSeq(tail)) { _ +: _}
				case _				=> Parser success Vector.empty
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
				def loop(input2:ParserInput[S], look:Seq[S]):ParseResult[S,Seq[S]]	=
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
}

abstract class Parser[S,+T] { self =>
	def parse(input:ParserInput[S]):ParseResult[S,T]

	//------------------------------------------------------------------------------

	def when(pred:Predicate[T]):Parser[S,Option[T]]	=
			self map { it =>
				pred(it) option it
			}

	def ensure(pred:Predicate[T]):Parser[S,T]	=
			require(_ optionBy pred)

	def requiredFor[U](name:String)(implicit ev:T=>Option[U]):Parser[S,U]	=
			required.named(name)

	def required[U](implicit ev:T=>Option[U]):Parser[S,U]	= require(ev)

	def requirePartial[U](func:PartialFunction[T,U]):Parser[S,U]	= require(func.lift)

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


	// TODO add child names
	def orElse[TT>:T](that:Parser[S,TT]):Parser[S,TT]	=
			input => {
				(self parse input, that parse input) match {
					case (Failure(i1, e1),	Failure(_, _))		=> Failure(i1, e1)
					case (Success(i1, s1),	Failure(_, _))		=> Success(i1, s1)
					case (Failure(_, _),	Success(i2, s2))	=> Success(i2, s2)
					case (Success(i1, s1),	Success(_, _))		=> Success(i1, s1)
				}
			}

	// TODO add child names
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
			flatMap(ev)

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
			tag(())

	def left(that:Parser[S,Any]):Parser[S,T]	=
			self next that map { _._1 }

	def right[U](that:Parser[S,U]):Parser[S,U]	=
			self next that map { _._2 }

	//------------------------------------------------------------------------------

	// TODO add child names
	def option:Parser[S,Option[T]]	=
			input => {
				self parse input match {
					case Success(input1, t)	=> Success(input1,	Some(t))
					case Failure(_, _)		=> Success(input,	None)
				}
			}

	def seq:Parser[S,Seq[T]]	= vector

	// TODO add child names
	def vector:Parser[S,Vector[T]]	=
			input => {
				@tailrec
				def loop(input1:ParserInput[S], accu:Vector[T]):ParseResult[S,Vector[T]]	=
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
				def loop(input1:ParserInput[S], accu:Seq[T]):ParseResult[S,Seq[T]]	=
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
			sepNes(sepa) map { _.toVector } orElse (Parser success Vector.empty)

	def sepNes(sepa:Parser[S,Any]):Parser[S,Nes[T]]	=
			self next (sepa right self).seq map { case (x, xs) => Nes(x, xs) }

	//------------------------------------------------------------------------------

	// TODO add child names
	def inside(quote:Parser[S,Any]):Parser[S,T]	=
			quote right self left quote

	// TODO add child names
	def flag:Parser[S,Boolean]	=
			self.option.map(_.isDefined)

	//------------------------------------------------------------------------------

	def guards:Parser[S,Unit]	=
			prevents.prevents

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
			eatRight(ws).phrase

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
