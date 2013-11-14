package HiC


class chromMatrix(val size:Int,val chromLen:Map[String,Int],val chromNames:List[String]) {
  val totallength = chromLen.values.foldLeft(0:Long)(_+_)
  val perpx = totallength*1.0 / size
  val chromMin = List.range(0,chromNames.length).map(List.range(0,_).foldLeft(0:Long)((a:Long,b:Int)=>a+chromLen(chromNames(b))))
  println(chromMin)
  println("each px represents about " + perpx.toInt/1000 + "Kbp")
  val px = Array.ofDim[Int](size,size)
  val chNameMap = List.range(0,chromNames.length).foldLeft(Map[String,Int]().empty)((a,b)=>a+Pair(chromNames(b),b))
  println(chNameMap)
  def update(ch1:String,pos1:Int,ch2:String,pos2:Int){
    px(((chromMin(chNameMap(ch1))+pos1)/perpx).toInt)(((chromMin(chNameMap(ch2))+pos2)/perpx).toInt)  += 1
  }
}
