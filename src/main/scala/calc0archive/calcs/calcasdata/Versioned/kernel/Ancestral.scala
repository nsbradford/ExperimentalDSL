package archive.calcs.calcasdata.Versioned.kernel

/**
  * Analogue to Escher.measure.Traceable
  */
trait Ancestral {
  def ancestors: Set[Ancestral]
}

trait NoAncestors extends Ancestral {
  override final def ancestors: Set[Ancestral] = Set()
}

trait OneAncestor extends Ancestral {
  def ancestor: Ancestral
  override final def ancestors: Set[Ancestral] = Set()
}

trait TwoAncestors extends Ancestral {
  def a: Ancestral
  def b: Ancestral
  override final def ancestors: Set[Ancestral] = Set(a, b)
}
