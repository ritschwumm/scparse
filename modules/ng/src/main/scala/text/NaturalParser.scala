package scparse.ng.text

object NaturalParser {
	// TODO have names

	def natural:TextParser[BigInt]		= naturalNZ orElse naturalZ

	def naturalNZ:TextParser[BigInt]	= digitNZ cons digit.list map { buildNumber(10, decodeNumber, _) }
	def naturalZ:TextParser[BigInt]		= digitZ tag 0L

	val digit:TextParser[Char]			= CharParser.digit
	def digitNZ:TextParser[Char]		= digit filter { _ != '0' } named "non-zero digit"
	def digitZ:TextParser[Char]			= TextParser is '0'			named "zero digit"

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
