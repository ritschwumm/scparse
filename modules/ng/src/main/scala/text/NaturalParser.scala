package scparse.ng.text

import scparse.ng.text.CharParser._

object NaturalParser {
	def natural:TextParser[BigInt]		= naturalNZ orElse naturalZ

	def naturalNZ:TextParser[BigInt]	= digitNZ cons digit.list map { buildNumber(10, decodeNumber, _) }
	def naturalZ:TextParser[BigInt]		= digitZ tag 0L

	def digitNZ:TextParser[Char]		= digit filter { _ != '0' }
	def digitZ:TextParser[Char]			= TextParser is '0'

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
