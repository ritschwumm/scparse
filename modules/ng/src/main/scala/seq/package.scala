package scparse.ng.seq

import scparse.ng.*

implicit final class SeqParserParseOps[S,T](peer:Parser[S,T]) {
	def parseIndexedSeq(s:IndexedSeq[S]):ParserResult[S,T]	=
		peer parse (IndexedSeqInput of s)

	def parseList(s:List[S]):ParserResult[S,T]	=
		peer parse (ListInput of s)
}
