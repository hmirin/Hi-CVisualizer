package HiC

import processing.core
import scala.annotation.tailrec
import scala.io.Source
import HiC.chromMatrix
import scala.Char
import java.util.StringTokenizer
import processing.core.{PApplet, PConstants}

class View extends processing.core.PApplet {
  val chromLen = Source.fromFile("chromo.csv").getLines().toArray.foldLeft(Map[String,Int]().empty)((m:Map[String,Int],s:String)=>m+Pair(s.split(',')(0),s.split(',')(1).toInt))
  println("chromosome data")
  val chromNames = chromLen.keySet.toList.sorted
  for (l <-chromNames) {
    println(l + "  " +chromLen(l).toString)
  }
  val totallength = chromLen.values.foldLeft(0:Long)(_+_)
  println("total length: " + totallength  )
  val windowsize = 500
  val perpx = totallength*1.0 / windowsize
  println("each px represents about " + perpx.toInt/1000 + "Kbp")
  val px = new chromMatrix(windowsize,chromLen,chromNames)

  override def setup {
    size(px.size, px.size)
    colorMode(processing.core.PConstants.RGB, 100)
    frameRate(999)
    background(100)
    noLoop
  }

  override def draw {
    var i=0
    for (l <- Source.fromFile("---datafile---").getLines()) {
      if ((i/100)*100==i) {
        println(i)
      }
      i+=1
      val p = l split '\t'
      //println(p)
      try{
        px.update(p(0),p(1).toInt,p(2),p(3).toInt)
      } catch {
        case e:NumberFormatException => { println("error with: "+l); }
        case e:ArrayIndexOutOfBoundsException => {
          if (p(0).isEmpty) {
            println( "extra line")
          }
          else sys.exit(1)
        }
      }
    }
    println("matrix update done")
    val maximum = px.px.foldLeft(0)((a:Int,b:Array[Int])=>math.max(a,b.foldLeft(0)(math.max(_,_))))
    println("maximum px: "+maximum)
    var k=1
    for (i <- List.range(0,windowsize)){
      for (j <- List.range(0,windowsize)) {
        stroke(100.0.toFloat,0.0.toFloat,0.0.toFloat,(math.log(px.px(i)(j))*1.0/math.log(maximum)*100).toFloat) //log
        point(i,j)
        point(j,i)
      }
      if ((k/100)*100==k) {
        println(k)
      }
      k+=1
    }
    for (i <-px.px){
      println(i.toList)
    }
  }
}

object Main {
  def main(args:Array[String]) {
    PApplet.main("HiC.View")
  }
}
