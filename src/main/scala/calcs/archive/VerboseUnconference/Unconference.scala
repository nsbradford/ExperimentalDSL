package calcs.archive.VerboseUnconference

/**
  * ============================================================================
  * Hello!
  * ============================================================================
  *
  * nsbradford@gmail.com
  * www.nicholasbradford.io
  *
  * https://github.com/nsbradford/ExperimentalDSL
  *
  * Disclaimers:
  *   -Experimental
  *   -Forgot to use markdown, sorry :(
  *
  * What can you expect?
  *   -Problem context and interesting ideas
  *   -Detail requirements
  *     -and future nice-to-haves
  *   -Naive code
  *     ... and non-compiling ideas
  *   -Roasting
  *   -Chance for you to learn...by teaching
  *
  * What can you not expect?
  *   -A framework that already works (but a chance to help create one)
  *   -Learning new things about type theory
  *
  *
  */
object Unconference {

  /**
    * ============================================================================
    * Context and Motivation
    * ============================================================================
    *
    * It's a common desire: separate the platform from the biz logic.
    *   But a framework is born the same as any refactoring...
    *
    * Recently we notice a common pattern: Diagnosable Long Logic Pipelines
    *
    *   (Hydrate A) => B => C => D => E => F => (G => Log and persist)
    *
    * Is this mere function composition? No, we need to support:
    *   Sequential(A => B => D => E)
    *   Multi-use/caching(A => B, D)
    *   Parallel
    *     Split(A => B, C)
    *     Merge(A,B,C => D)
    *     Independence at Inception(A, E)
    *
    *     ------>
    *   A -> B -> D
    *     ->   ->
    *        C
    *   E ->
    *
    *
    * Is this mere DAG (Directed Acyclic Graph)? No, there are cycles (it's just a DG):
    *   Plenty of use-cases for tailrec algorithms.
    *
    *   A -> (B -> C) -> D
    *         ^ <- v
    *
    *
    * What else are we doing besides the computation?
    *   Lots and lots of side effects for diagnostic logging:
    *     Many pipeline steps have to manually persist diagnostics, heavy in boilerplate.
    *
    * Let's consolidate examples, assuming every step (->) has diagnostic side-effect.
    *
    *               ------>
    *   (Hydrate A) -> B -> D -> (F -> G) -> (H => Persist)
    *               ->   ->       ^ <- v
    *                  C
    *   (Hydrate E) ->
    *
    *
    * We want to compose the above side-effecting pipeline with minimal boilerplate:
    *
    *   // something like this?
    *   val f: (A, E) => H = ???
    *   val pipeline: Calc[A, E, H] = Calc("BizLogic0x03", f)
    *   pipeline.runWhilePersistingDiags(hydrateLatestA(), hydrateLatestE())
    *
    * Why not just a Calc[H]?
    *   Because we want to pass pipelines around and tie them together,
    *   which means we have to keep track of input types.
    *
    *
    *
    * Finally, always remember the first principle:
    *   Existing biz logic should stay basically unchanged.
    *
    *
    * ============================================================================
    * Detail Requirements
    * ============================================================================
    *
    * Is this just the same thing as Apache Airflow?
    *   Nope, for many reasons... though eventually there could be overlap.
    *
    *
    * See SharedModel.scala
    *
    * ... TODO
    *
    * ============================================================================
    * Naive Code
    * ============================================================================
    *
    * Why start here? Tried to jump right into combining State, IO, and HLists...
    *   ...didn't go great.
    *
    * Instead:
    *   0) Declare Calc1-22 for Function1-22
    *   1) Explicitly separate Calcs and Results
    *
    *   case class Agg1[-T1, +R](v1: CalcVersionAssigned, r: UnversionedData[R]) extends Agg[R]
    *   trait Calc1[-T1, +R] extends Function1[VersionedData[T1], Agg1[T1, R]] with Calc[R]
    *
    * ============================================================================
    * Ideas
    * ============================================================================
    *
    * ---------
    * Variance/Covariance rationalization
    *
    * We can't have Calc1[-T1, +R] because we need to pass ev of HasRepository[T1],
    *   which breaks because HasRepository[T] is invariant,
    *   and there's no reason to believe HasRepository should be contravariant.
    *
    * So, no variance - we just have Calc1[T1, +R].
    *
    *
    * ---------
    * Delete the Result type: combine execution + persistence for simplicity
    *
    *   trait Calc1[-T1, +R] extends Function1[VersionedData[T1], VersionedData[R]] with Calc[R]
    *
    *
    * ---------
    * Wait a sec! Isn't this just a _____ (functor, applicative, monad, ...)
    *
    * It has all the signs:
    *   -Sequencing computations (monad)
    *   -Parallel/independent computations (applicative)
    *   -mapping from VersionedData[A] => VersionedData[B] (functor?)
    *
    * ...if you could abstract over the arity of the functions (more on that later).
    *   Or maybe we can just ignore this and get our composability/etc from the effect type
    *
    * ---------
    * Effect Type: IO (worry about F[_] + Tagless Final later)
    *
    *   What this gives us:
    *     -Parallel/Async
    *       -(?) Pretty sure all the cases are covered with use of Applicative/mapN...
    *     -Feels "right"
    *       -Calcs are inherently side-effectful
    *       -IO gives us referential transparency!
    *     -Also, can replace the Try[Unit]/Try[Version] db operations with IO[Unit]/IO[Version]
    *       -IO has built-in exception handling with IO.raiseError(e) + IO.attempt
    *
    *   Where do we put it? Right smack in the Input/Output types:
    *
    *
    * ---------
    * eDSL w/ the Free Monad: Source/Sink/Run
    *
    *   Pros:
    *     Get the computation graph for free! Just write a "VizualizeGraph" interpreter
    *       Sort of - at least you get the non-recursive parts
    *         TBD I'm assuming modelling
    *     Run normalization steps/optimizers over the pipeline before running
    *       (unclear if comes up often in practice - though some other DAG libraries do this)
    *   Cons:
    *     Actually not sure how much Structure can be separated from Impl,
    *
    *
    * ---------
    * (Stretch) Abstract over arity of function args:
    *
    *   Probably means a Calculator is a function of HList => HList,
    *     and evidence of repositories is also an HList...
    *     ... a swift path to typeclass compile error hell?
    *
    *
    * ---------
    * (Stretch) State Accumulator: delay injection of HasRepository[T] until outer layer
    *
    *   Requires recording all the Inputs/outputs with their types.
    *
    *
    *
    * .... and more...
    * ---------
    * (Bonus) Implicit chaining
    *
    *   Unfortunately not on this computer, but if you want to calculate an [R], given
    *     implicit ev: Calc[(InputLayer1), R]
    *     implicit ev: Calc[(InputLayer2), (InputLayer1)]
    *     ...
    *     automatically derive the typeclass (i.e. with Shapeless), and then summon:
    *
    *     val derivedCalcTree: Calc[Unit, R] = Calc.summon[R]
    *
    *
    */
}
