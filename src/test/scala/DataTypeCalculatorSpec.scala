import org.scalatest._

class DataTypeCalculatorSpec extends FlatSpec with DiagrammedAssertions {

  it should "ユーザーデータ型の占有ワード数を算出可能" in {
    val typeAliasList: Seq[DataType] = Seq(
      TypeAlias("BOOL_ALIAS", "BOOL"),
      TypeAlias("INT_ALIAS", "INT"),
      TypeAlias("DWORD_ALIAS", "DWORD")
    )
    assert(DataTypeCalculator(typeAliasList) == Map(
      "BOOL_ALIAS" -> 1,
      "INT_ALIAS" -> 1,
      "DWORD_ALIAS" -> 2
    ))

    val typeArrayList: Seq[DataType] = Seq(
      TypeArray("BOOL_ARRAY1", 0, 0, "BOOL"),
      TypeArray("BOOL_ARRAY2", 0, 15, "BOOL"),
      TypeArray("BOOL_ARRAY3", 0, 16, "BOOL"),
      TypeArray("INT_ARRAY1", 0, 0, "INT"),
      TypeArray("INT_ARRAY2", 0, 999, "INT"),
      TypeArray("INT_ARRAY3", 1, 1000, "INT"),
      TypeArray("DINT_ARRAY1", 0, 0, "DINT"),
      TypeArray("DINT_ARRAY2", 0, 999, "DINT"),
      TypeArray("DINT_ARRAY3", 1, 1000, "DINT"),
    )
    assert(DataTypeCalculator(typeArrayList) == Map(
      "BOOL_ARRAY1" -> 1,
      "BOOL_ARRAY2" -> 1,
      "BOOL_ARRAY3" -> 2,
      "INT_ARRAY1" -> 1,
      "INT_ARRAY2" -> 1000,
      "INT_ARRAY3" -> 1000,
      "DINT_ARRAY1" -> 2,
      "DINT_ARRAY2" -> 2000,
      "DINT_ARRAY3" -> 2000
    ))


    val boolList00_15 = (0 to 15).map(e => TypeAlias("BOOL_ALIAS" + e.toString, "BOOL")).toList
    val boolList00_31 = (0 to 31).map(e => TypeAlias("BOOL_ALIAS" + e.toString, "BOOL")).toList
    val intList1_4 = (1 to 4).map(e => TypeAlias("INT_ALIAS" + e.toString, "INT")).toList
    val dintList1_3 = (1 to 3).map(e => TypeAlias("DINT_ALIAS" + e.toString, "DINT")).toList
    val typeStructList: Seq[DataType] = Seq(
      TypeStruct("BOOL_STRUCT1", Seq(TypeAlias("BOOL_ALIAS", "BOOL"))),
      TypeStruct("BOOL_STRUCT2", boolList00_15),
      TypeStruct("BOOL_STRUCT3", boolList00_31),
      TypeStruct("BOOL_STRUCT4", boolList00_31 :+ TypeAlias("BOOL_ALIAS32", "BOOL")),
      TypeStruct("INT_STRUCT1", intList1_4),
      TypeStruct("INT_STRUCT2", intList1_4 :+ TypeAlias("INT_ALIAS5", "INT")),
      TypeStruct("DINT_STRUCT1", dintList1_3),
      TypeStruct("DINT_STRUCT2", dintList1_3 :+ TypeAlias("DINT_ALIAS", "DINT")),
      TypeArray("BOOL_ARRAY1", 0, 31, "BOOL"),
      TypeArray("BOOL_ARRAY2", 0, 7, "BOOL"),
      TypeArray("BOOL_ARRAY3", 0, 7, "BOOL"),
      TypeArray("BOOL_ARRAY4", 0, 7, "BOOL"),
      TypeStruct("BOOL_ARRAY_STRUCT1", Seq(TypeAlias("BOOL_ARRAY_1", "BOOL_ARRAY1"))),
      TypeStruct("BOOL_ARRAY_STRUCT2", Seq(TypeAlias("BOOL_ARRAY_2", "BOOL_ARRAY2"), TypeAlias("BOOL_ARRAY_3", "BOOL_ARRAY3"), TypeAlias("BOOL_ARRAY_4", "BOOL_ARRAY4")))
      )

    assert(DataTypeCalculator(typeStructList) == Map(
      "BOOL_STRUCT1" -> 2,
      "BOOL_STRUCT2" -> 2,
      "BOOL_STRUCT3" -> 2,
      "BOOL_STRUCT4" -> 4,
      "INT_STRUCT1" -> 4,
      "INT_STRUCT2" -> 6,
      "DINT_STRUCT1" -> 6,
      "DINT_STRUCT2" -> 8,
      "BOOL_ARRAY1" -> 2,
      "BOOL_ARRAY2" -> 1,
      "BOOL_ARRAY3" -> 1,
      "BOOL_ARRAY4" -> 1,
      "BOOL_ARRAY_STRUCT1" -> 2,
      "BOOL_ARRAY_STRUCT2" -> 4
    ))

  }
}
