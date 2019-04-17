package calcs.edsl1

/**
  * Created by nicholasbradford on 4/17/19.
  */
object EDsl1 {

  /**
    * Operations:
    *
    *
    *
    * ChainLift: takes a chain and gives it a name/identity
    * Compose/Chain: composes two calcs and returns an unnamed new calc
    *   This is necessary so that every set of pipeline joins isn't it's own calculator
    *     ... or is it?
    *   maybe it's exactly what you want: this way each sub-group of the VLK pipeline
    *     is its own calc that's easy to re-run...
    *     ...but
    *
    * RecursiveCalc: a self-referential calc that may reference itself
    */



}