package scparse.ng

package object seq {
	implicit class SeqParserParseOps[S,T](peer:Parser[S,T]) {
		def parseIndexedSeq(s:IndexedSeq[S]):ParserResult[S,T]	=
			peer parse (IndexedSeqInput of s)

		def parseList(s:List[S]):ParserResult[S,T]	=
			peer parse (ListInput of s)
	}
}
