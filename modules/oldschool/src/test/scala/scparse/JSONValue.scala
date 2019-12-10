package scparse.oldschool.demo

sealed abstract class JSONValue

case object JSONNull										extends JSONValue
final case class JSONBoolean(value:Boolean)					extends JSONValue
final case class JSONNumber(value:BigDecimal)				extends JSONValue
final case class JSONString(value:String)					extends JSONValue
final case class JSONArray(value:Seq[JSONValue])			extends JSONValue
final case class JSONObject(value:Seq[(String,JSONValue)])	extends JSONValue
