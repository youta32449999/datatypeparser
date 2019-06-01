object Main extends App {
  val file = scala.io.Source.fromFile("./datatypeTest.txt")
  val parseResultSeq: Seq[DataType] = DataTypeParser(file.mkString) match {
    case DataTypeParser.Success(result, _) => result
    case _ => Nil
  }

  val result = DataTypeCalculator("./PrimitiveDataType.txt", parseResultSeq)
  println(result)


}