package scparse.oldschool

import scala.language.implicitConversions

import java.io.File

object Source {
	implicit def ListAsSource[C](in:List[C]):Source[C]				= ListSource		of in
	implicit def SeqAsSource[C](in:Seq[C]):Source[C]				= SeqSource			of in
	implicit def IndexedSeqAsSource[C](in:IndexedSeq[C]):Source[C]	= IndexedSeqSource	of in
	implicit def ArrayAsSource[C](in:Array[C]):Source[C]			= IndexedSeqSource	of in.toIndexedSeq
	implicit def StringAsSource(in:String):Source[Char]				= StringSource		of in
	implicit def FileAsSource(in:File):Source[Byte]					= MappedFileSource	of in
}

/** input to a Parser */
trait Source[+C] {
	def cata[X](empty: =>X, full:(Source[C],C)=>X):X

	def isEmpty:Boolean		= cata(true, (_,_)=>false)
	def nonEmpty:Boolean	= !isEmpty
}
