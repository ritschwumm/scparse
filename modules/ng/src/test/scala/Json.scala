package scparse.ng.demo

/*
object Json {
	case object Null									extends Json
	final case class Boolean(value:scala.Boolean)		extends Json
	final case class Number(value:BigDecimal)			extends Json
	final case class String(value:java.lang.String)		extends Json
	final case class Array(value:Seq[Json])				extends Json
	final case class Object(value:Seq[(java.lang.String,Json)])	extends Json
}

sealed abstract class Json
*/

enum Json {
	case Null
	case Boolean(value:scala.Boolean)
	case Number(value:BigDecimal)
	case String(value:java.lang.String)
	case Array(value:Seq[Json])
	case Object(value:Seq[(java.lang.String,Json)])
}
