package archive.calcs.calcasdata.Versioned.kernel


/**
  * A location where a conceptual type of data is held,
  *   at more granularity than a Scala type.
  * For example, you might have multiple Addresses for
  *   different conceptual types of Ints that you want to store.
  *
  * Somewhat analogous to Escher's use of address,
  *   except this is imbued into the Metadata instead of assigned inline during logic,
  *   which [hopefully] leads to better consistency.
  */
case class Address(name: String) // TODO use macro or classpath to eliminate collisions?


/**
  * A producer of data.
  */
case class CalcName(name: String) // TODO force auto-generation from classpath?


/**
  * An atomic unit of data production.
  * CalcRuns are MANY-TO-MANY with Inputs and Outputs.
  */
case class CalcRun(calcName: CalcName, calcRunId: Long)


/**
  * TODO unsure about the "anonymous calc" approach
  *   but may be a promising way to describe unversioned data.
  * Other paths might include:
  *   1) Keep an "anonymous" Calc, but request a new ID every time for unique ID.
  *   2) Make the calcname the Hash of the unversioned data
  */
object CalcRun {
  def apply(name: String, calcRunId: Long): CalcRun = CalcRun(CalcName(name), calcRunId)

  private val anonymous: CalcName = CalcName("Anonymous")
  private val unknownRunId: Long = -1
  val unknown: CalcRun = CalcRun(anonymous, unknownRunId)
}


/**
  * All the metadata requires to uniquely identify a piece of data.
  *   This "piece" need not be a primitive;
  *   it could be a Seq, Map, arbitrary Product type, etc;
  *   no constraints are imposed.
  */
case class DataVersion(producer: CalcRun, address: Address)
