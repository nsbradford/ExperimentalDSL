package calcs.calcasdata.Versioned.kernel


/**
  * Analogue to Escher HasMetadata.
  */
trait NamedMetadata {

  // TODO only useful from a viz+diagnostics perspective
  def variableName: Option[String] = None

}
