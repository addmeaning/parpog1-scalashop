def mapASegSeq[A, B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]) = {
  var i = left
  while (i < right) {
    out(i) = f(inp(i))
    i += 1
  }
}
val in = Array(2, 3, 4, 5, 6)
val out = Array(0, 0, 0, 0, 0)
//val f = (x: Int) => x * x
//mapASegSeq(in, 1, 3, f, out)
out
val threshold = 50
//def mapASegPar[A, B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]) = {
//  if (right - left < threshold) {
//    mapASegSeq(inp, left, right, f, out)
//  } else {
//    val mid = left + (right - left) / 2
//    parallel(mapASegPar(inp, left, mid, f, out),
//      mapASegPar(inp, mid, right, f, out))
//  }
//
//}
def f(u: Double, v: Double): Double =(u + v)/(1.0 + u*v)
def err(lst:List[Double]): Double =
  lst.reduceLeft(f) - lst.reduceRight(f)
def testAssoc: Double = {
  val r = new scala.util.Random
  val lst = List.fill(400)(r.nextDouble*0.002)
  err(lst)
}

testAssoc