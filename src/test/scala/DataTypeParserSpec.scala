import org.scalatest._

class DataTypeParserSpec extends FlatSpec with DiagrammedAssertions {
  import DataTypeParser._

  "comment" should "コメント定義をパース可能" in {
    assert(parseAll(comment, "(* コメント定義です! *)") match {
      case Success(result, _) => result == "(* コメント定義です! *)"
      case _ => false
    })

    // コメントに*を使用可能
    assert(parseAll(comment, "(*****コメント*********)") match {
      case Success(result, _) => result == "(*****コメント*********)"
      case _ => false
    })

    // コメントに()を使用可能
    assert(parseAll(comment, "(*(コメント)*)") match {
      case Success(result, _) => result == "(*(コメント)*)"
      case _ => false
    })
  }

  "digits" should "1個以上の半角数字の列をパース可能" in {
    assert(parseAll(digits, "192837465") match {
      case Success(result, _) => result == "192837465"
      case _ => false
    })

    assert(parseAll(digits, "") match {
      case Success(_, _) => false
      case _ => true
    })

    assert(parseAll(digits, "００８３") match {
      case Success(_, _) => false
      case _ => true
    })

    assert(parseAll(digits, "0083StarDustMemorys") match {
      case Success(result, _) => false
      case _ => true
    })
  }

  "dataTypeName" should "数字または半角英数字の1個以上の文字列をパース可能" in {
    assert(parseAll(dataTypeName, "kanikani83") match {
      case Success(result, _) => result == "kanikani83"
      case _ => false
    })
  }

  "typeAlias" should "型エイリアスの定義をパース可能" in {
    assert(parseAll(typeAlias, "RichBool : Bool;") match {
      case Success(result, _) => result == TypeAlias("RichBool", "Bool")
      case _ => false
    })
  }

  "array" should "配列の定義をパース可能" in {
    assert(parseAll(array, "kani : ARRAY [1..100] OF BOOL;") match {
      case Success(result, _) => result == TypeArray("kani", 1, 100, "BOOL")
      case _ => false
    })
  }

  "struct" should "構造体の定義をパース可能" in {
    val input1 = """
                   |NODE :
                   | STRUCT
                   |   value : INT ;
                   |   left : NODE ;
                   |   right : NODE;
                   | END_STRUCT ;
                   | """.stripMargin
    assert(parseAll(struct,input1) match {
      case Success(result, _) => result == TypeStruct("NODE", List(TypeAlias("value", "INT"), TypeAlias("left", "NODE"), TypeAlias("right", "NODE")))
      case _ => false
    })
  }

  it should "データ型定義をパース可能" in {
    val input = """
      (* ユーザー定義型宣言 *)
      TYPE
        NODE : (* 木のノード *)
          STRUCT
            VALUE : INT ;
            LEFT : NODE (* 左の子 *);
            (* 右の子 *)RIGHT : NODE ;
          END_STRUCT;

        BUFFER : ARRAY[1..999] OF REAL;

        ID : STRING;

      END_TYPE""".stripMargin

    assert(DataTypeParser(input) match {
      case Success(result, _) => result ==
        List(
          TypeStruct("NODE",
            List(
              TypeAlias("VALUE", "INT"),
              TypeAlias("LEFT", "NODE"),
              TypeAlias("RIGHT", "NODE")
            )
          ),
          TypeArray("BUFFER", 1, 999, "REAL"),
          TypeAlias("ID", "STRING"))
      case _ => false
    })
  }
}
