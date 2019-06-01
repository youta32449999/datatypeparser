import scala.util.parsing.combinator._

sealed trait DataType
case class TypeAlias(name: String, elementDataType: String) extends DataType
case class TypeArray(name: String, firstIndex: Int, lastIndex: Int, elementDataType: String) extends DataType
case class TypeStruct(name: String, member: Seq[DataType]) extends DataType

object DataTypeParser extends JavaTokenParsers {

  // コメントにマッチする正規表現
  private[this] val commentRegex = "\\(\\*.*\\*\\)"

  def comment: Parser[String] = "^\\(\\*.*\\*\\)$".r

  def digits: Parser[String] = "[0-9]+".r

  def dataTypeName: Parser[String] = "[0-9A-z]+".r

  def typeAlias: Parser[DataType] = dataTypeName ~ ":" ~ dataTypeName ~ ";" ^^
    {case name ~ _ ~ elementDataType ~ _ => TypeAlias(name, elementDataType)}

  def array: Parser[DataType] = dataTypeName ~ ":" ~ "ARRAY" ~ "[" ~ digits ~ ".." ~ digits ~ "]" ~ "OF" ~ dataTypeName ~ ";" ^^
    {case name ~ _ ~ _ ~ _ ~ firstIndex ~ _ ~ lastIndex ~ _ ~ _ ~ elementDataType ~ _ => TypeArray(name, firstIndex.toInt, lastIndex.toInt, elementDataType)}

  def struct: Parser[DataType] = dataTypeName ~ ":" ~ "STRUCT" ~ rep(typeAlias | array) ~ "END_STRUCT" ~ ";" ^^
    {case name ~ _ ~ _ ~ member ~ _ ~ _ => TypeStruct(name, member)}

  def dataType: Parser[Seq[DataType]] = "TYPE" ~ rep(typeAlias | array | struct) ~ "END_TYPE" ^^
    {case _ ~ list ~ _ => list}

  // inputからコメントを除去してからパース開始
  def apply(input: String): ParseResult[Seq[DataType]] = parseAll(dataType, input.replaceAll(commentRegex, ""))

}



// データ型のBNF
//  <comment> ::= "^\(\*.*\*\)$"
//  <digits> ::= "[0-9]+"
//  <data-type-name> ::= "[0-9A-z]+"
//  <type-alias> ::= <data-type-name> ":" <data-type-name> ";"
//  <array> ::= <data-type-name> ":" "ARRAY[" <digits> ".." <digits> "]" "OF" <data-type-name> ";"
//  <data-type> ::= "TYPE" {<structure>} "END_TYPE"
//  <structure> ::= <array> | <struct>
//  <struct> ::= <data-type-name> ":" "STRUCT" {<member-name> ":" <data-type-name> ";"} "END_STRUCT" ";"

