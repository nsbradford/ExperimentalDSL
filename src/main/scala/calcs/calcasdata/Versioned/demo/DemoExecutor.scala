package calcs.calcasdata.Versioned.demo

import calcs.calcasdata.Versioned.kernel._


class DemoExecutor extends Executor {
  override type Result[T] = T

  override def run[T](pure: Pure[T]): T = pure.raw

  override def run[T](lazyV: LazyV[T]): T = lazyV.run(this)

  override def run[A, T](map: Map[A, T]): T = {
    map.f(map.ancestor.run(this))
  }

  override def run[A, T](fMap: FMap[A, T]): T = {
    fMap.f(fMap.ancestor.run(this)).run(this)
  }

  override def run[A, B, T](combine: Combine[A, B, T]): T = {
    combine.f(combine.a.run(this), combine.b.run(this))
  }

  override def run[T](procedure: Procedure[T]): T = {
    println(s"Entering procedure ${procedure.name}")
    println(s"logging these inputs: ${procedure.inputs}")
    println(s"Executing...")
    val result = procedure.result.run(this)
    println(s"Publishing result... : $result")
    result
  }
}