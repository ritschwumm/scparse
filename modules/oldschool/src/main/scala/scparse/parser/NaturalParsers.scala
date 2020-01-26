package scparse.oldschool

/** parsing of natural numbers in Strings */
trait NaturalParsers[M[+_]] { self:Parsers[M] with StringParsers[M] =>
	def natural:StringParser[BigInt]	= naturalNZ alternate naturalZ

	def naturalNZ:StringParser[BigInt]	= digit.repeat cons digitNZ map { buildNumber(10, decodeNumber, _) }
	def naturalZ:StringParser[BigInt]	= digitZ tag 0L

	def digitNZ:StringParser[Char]	= digit filter { _ != '0' }
	def digitZ:StringParser[Char]	= self accept '0'

	def buildNumber(placeValue:BigInt, digitValue:Char=>BigInt, digitChars:Seq[Char]):BigInt	=
		digitChars.foldLeft(BigInt(0)){
			(out,v) => out * placeValue + digitValue(v)
		}

	val decodeNumber:PartialFunction[Char,BigInt]	= {
		case c if c >= '0' && c <= '9'	=> c - '0'
		case c if c >= 'a' && c <= 'z'	=> c - 'a' + 10
		case c if c >= 'A' && c <= 'Z'	=> c - 'A' + 10
	}
}
