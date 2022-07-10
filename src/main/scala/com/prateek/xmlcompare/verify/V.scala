package com.prateek.xmlcompare.verify

trait V

class NV(_vs: => Seq[V]) extends V:
  lazy val vs: Seq[V] = _vs

class CV(_v: => V) extends V:
  lazy val v: V = _v

object Main extends App {
  val nv: NV = NV(vs)

  println(vs)
  val cv: CV = CV(nv)

  def vs: Seq[V] = Seq(cv)

  println(s"$nv ${nv.vs} ")
  println(s"$cv ${cv.v}")
}
