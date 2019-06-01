import java.awt.Component

import scala.annotation.tailrec

object DataTypeCalculator {

  // data type info from parse result
  private[this] case class Data(name: String, component: Seq[DataTypeSize], arrayLength: Int, isStruct: Boolean) {
    // データ型のサイズに０がある場合は常に０を返す
    def size: Int = {
      val sum = component.foldLeft(0){(acc, e) => e.size + acc} * arrayLength
      val resultBuff = if(sum % toCalForm(1) == 0) sum else toCalForm((sum / toCalForm(1))) + toCalForm(1)
      val result = if(isStruct && resultBuff % toCalForm(2) != 0) resultBuff + toCalForm(1) else resultBuff
      val containsZero = component.filter(e => e.size == 0) != Nil
      if (containsZero) 0 else result
    }
  }

  private[this] case class DataTypeSize(name: String, size: Int)


  private[this] val primitiveDataTypeSize = Seq(
    DataTypeSize("BOOL", toCalForm("0.0625")),
    DataTypeSize("WORD", toCalForm("1")),
    DataTypeSize("INT", toCalForm("1")),
    DataTypeSize("UINT", toCalForm("1")),
    DataTypeSize("DWORD", toCalForm("2")),
    DataTypeSize("DINT", toCalForm("2")),
    DataTypeSize("UDINT", toCalForm("2")),
    DataTypeSize("REAL", toCalForm("2")),
    DataTypeSize("TIME", toCalForm("2"))
  )


  // 計算用に小数点以下桁上げ
  private[this] def toCalForm(datasize: String): Int = (datasize.toFloat * 10000).toInt


  private[this] def toCalForm(datasize: Int): Int = (datasize * 10000)


  //　計算用から実際のサイズに戻す
  private[this] def fromCalForm(datasize: Int): Int = datasize / 10000


  // parse result convert for DataType
  private[this] def convertToData(seq: Seq[DataType]): Seq[Data] = seq.map(e => e match {
    case TypeAlias(name, datatype) => Data(name, Seq(DataTypeSize(datatype, 0)), 1, false)
    case TypeArray(name, firstIndex, lastIndex, datatype) => Data(name, Seq(DataTypeSize(datatype, 0)), lastIndex - firstIndex + 1, false)
    case TypeStruct(name, member) => Data(name, convertToData(member).map(e => e.component).flatten, 1, true)
  })

  private[this] def calculate(parseResultSeq: Seq[DataType]): Seq[Data] = {
    @tailrec
    def loop(sizeSeq: Seq[DataTypeSize], dataSeq: Seq[Data]): Seq[Data] = sizeSeq match {
      case x::xs => {
        // update data type size in component
        val newDataSeq = dataSeq.map{e =>
          val newComponent = e.component.map{c => if(c.name == x.name) c.copy(size = x.size) else c}
          e.copy(component = newComponent)
        }

        // サイズが決まったデータ型を追加する。無ければ追加なし
        val dicidedSizeList = newDataSeq.zip(dataSeq)
          .filter{case (newSeqElem, oldSeqElem) => newSeqElem.size != 0 && oldSeqElem.size == 0}
          .map{case (n,_) => DataTypeSize(n.name, n.size)}
        val newSizeSeq = xs ++ dicidedSizeList
        loop(newSizeSeq, newDataSeq)
      }
      case Nil => dataSeq
    }

    val dataList = convertToData(parseResultSeq)
    loop(primitiveDataTypeSize, dataList)
  }

  def apply(parseResultSeq: Seq[DataType]): Map[String, Int] = calculate(parseResultSeq)
    .map(e => e.name -> fromCalForm(e.size))
    .toMap
}