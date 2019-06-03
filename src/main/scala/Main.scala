object Main extends App {
  val userDataType =
    """
      |TYPE
      |
      | T_SAMPLE1	:	(* サンプルデータ型１ *)
      |	STRUCT
      |		sample1	:	DINT;	(* サンプル数値1*)
      |		sample2	:	INT;	(* サンプル数値2 *)
      |		sampleBit01	:	T_BOOL_ALIAS;	(* サンプルビット1 *)
      |   sampleBit02	:	T_BOOL_ALIAS;	(* サンプルビット2 *)
      |   sampleBit03	:	T_BOOL_ALIAS;	(* サンプルビット3 *)
      |   sampleBit04	:	T_BOOL_ALIAS;	(* サンプルビット4 *)
      |   sampleBit05	:	T_BOOL_ALIAS;	(* サンプルビット5 *)
      |   sampleBit06	:	T_BOOL_ALIAS;	(* サンプルビット6 *)
      |   sampleBit07	:	T_BOOL_ALIAS;	(* サンプルビット7 *)
      |   sampleBit08	:	T_BOOL_ALIAS;	(* サンプルビット8 *)
      |   sampleBit09	:	T_BOOL_ALIAS;	(* サンプルビット9 *)
      |   sampleBit10	:	T_BOOL_ALIAS;	(* サンプルビット10 *)
      |   sampleBit11	:	T_BOOL_ALIAS;	(* サンプルビット11 *)
      |   sampleBit12	:	T_BOOL_ALIAS;	(* サンプルビット12 *)
      |   sampleBit13	:	T_BOOL_ALIAS;	(* サンプルビット13 *)
      |   sampleBit14	:	T_BOOL_ALIAS;	(* サンプルビット14 *)
      |   sampleBit15	:	T_BOOL_ALIAS;	(* サンプルビット15 *)
      |   sampleBit16	:	T_BOOL_ALIAS;	(* サンプルビット16 *)
      |   sampleBit17	:	T_BOOL_ALIAS;	(* サンプルビット17 *)
      |	END_STRUCT;
      |
      |	T_SAMPLE_ARRAY			:	ARRAY[1..10]	OF	T_SAMPLE1	;
      |
      | T_BOOL_ALIAS  : BOOL  ;
      |
      |END_TYPE
    """.stripMargin
  val parseResultSeq: Seq[DataType] = DataTypeParser(userDataType) match {
    case DataTypeParser.Success(result, _) => result
    case _ => Nil
  }

  val result = DataTypeCalculator(parseResultSeq)
  println(result)
}
