package calcs

/**
  * ============================================================================
  * Hello!
  * ============================================================================
  *
  * What can you expect?
  *   -Problem context and interesting ideas
  *   -Detail requirements
  *     -and future nice-to-haves
  *   -Naive code that doesn't work
  *   -Opportunities for ideas
  *   -Chance for you to learn...by teaching
  *
  * What can you not expect?
  *   -Working code
  *   -Learning new things about type theory
  *
  */
object Unconference {

  /**
    * ============================================================================
    * Context
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
    * Is this mere DAG (Directed Acyclic Graph)? No, there are cycles (it's just a DG):
    *   Plenty of use-cases for tailrec algorithms.
    *
    *   A -> (B -> C) -> D
    *         ^ <- v
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
    * We want to compose the above side-effecting pipeline with minimal boilerplate:
    *
    *   // something like this?
    *   val pipeline: Calc[A, E, H] = ???
    *   pipeline.runWhilePersistingDiags(hydrateLatestA(), hydrateLatestE())
    *
    * Why not just a Calc[H]?
    *   Because we want to pass pipelines around and tie them together.
    *
    * But always remember the first principle:
    *   Existing biz logic should stay basically unchanged.
    *
    *
    * ============================================================================
    * Detail Requirements
    * ============================================================================
    *
    * ... TODO
    *
    */
}
