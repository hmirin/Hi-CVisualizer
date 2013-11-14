package HiC

class Calculator(val func:(Double=>Double)) {
  def start(init:Double, cycle:Int) :List[Double] = {
    def rec(cycle:Int, before:Double, answer:List[Double]) :List[Double] = {
      cycle match {
        case 0 => answer:+before
        case _ => {
          rec(cycle-1,func(before),answer:+before)
        }
      }
    }
    rec(cycle,init,List[Double]())
  }
}
