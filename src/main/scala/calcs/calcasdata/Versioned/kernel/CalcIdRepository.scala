package calcs.calcasdata.Versioned.kernel

import scala.util.Try


trait CalcIdRepository{
  def requisitionNewRunId(calcName: CalcName): Try[CalcRun]
}
