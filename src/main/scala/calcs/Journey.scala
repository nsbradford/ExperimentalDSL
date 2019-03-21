package calcs

/**
  * For the calculator pattern, we have some goals:
  *   -Composable, i.e. equivalent to Function1
  *   -Parallel/Async capabilities
  *   -Diagnosable
  *
  * And we have some peripheral goals that maybe we'll achieve as a by-product:
  *   -Indifferent between Calculating and Loading, allowing different options for
  *   -Version Ids
  *     -Calculators produce version IDs
  *     -Logic is agnostic to version ID
  *
  * And some crazy goals that we can safely ignore:
  *   -Implicit assembly: Given Calc[Unit, I], can build Calc[I,O] automatically
  *     -Means complex pipeline composition can occur
  *     -Major downside: you can't inspect the pipeline you built.
  *       -Also, unlikely you ever get a pipeline so large that this becomes necessary.
  *
  * here's a simple control flow that hits Sequential, Split, Merge, and Multi-use:
  *     ------>
  *   A -> B -> D -> Persist
  *     -> C ->
  *
  * And an alternative version of the pipe, which reads from a snapshot instead of recomputing:
  *     --------->
  *   A ->  B   -> D -> Persist
  *      CRunId ->
  *
  * And a logical inconsistent version, which uses different versions of the same data:
  *      ARunId ->
  *   A ->  B   -> D -> Persist
  *      -> C ->
  *
  * Here's a bunch of tools in the toolbox for achieving this:
  *   Just raw Function1
  *   Futures
  *   IO
  *   Reader
  *   Writer
  *   State
  *   ReaderWriterState
  *   Free Monad
  *   DAG (directional acyclic graphs), e.g. such as Stripe's Dagon
  *
  * Some reading on "Git for data" and versioning:
  *   ... basically found nothing useful, just a bunch of Git extensions
  *
  */
trait Journey {

  // Let's start just defining the ins-outs of our pipeline.

  /**
    * Prinicples for Versioned Data:
    *
    * Version numbers increase over time.
    * Version numbers are unique to a Conceptual Model
    *   (?) VersionNumber=(ModelVersion and DataVersion)?
    *   (?) OR have new iterations/migrations of a conceptual model start from scratch.
    *
    * Version numbers (incidentally, but not by decree) are at roots of all Calcs (in practice),
    *   if it's the latest you simply pipe in `getLatestVersion[T]` or `getLatestValidVersion[T]`,
    *   when simulating you might to `getLatestValidVersionByDate[T](dateTime)`.    *
    * This is because, in practice, you're probably never going to be generating data from Unit -
    *   what would that even mean?
    * Example:
    *   `getLatestValidVersion[HeldPositions] andThen hydrate[HeldPositions] andThen calculateMetrics`
    */
  trait Version
  case class VersionN(n: Int) extends Version //
  case object Latest extends Version

  trait Aout
  trait Bout
  trait Cout
  trait Dout
  trait Eout

  /**
    * Here's another place we have to decide: is a Type alone enough to get the latest version,
    *   or do we also need the type/model version? Do we solve that with Tagged Types?
    * Or do we use a simple name/symbol identifier to map real things to types?
    */
  def getLatestVersion[T]: Version // would need reflection/ClassTag
  def getLatestVersion(name: String): Version
  def getLatestVersion(name: String, modelVersion: Int): Version
  def getLatestVbyDate[T](date: Int): Version // etc.

  val aHydrate: Version => Aout
  val aHydrateLatest: () => Aout = () => aHydrate(getLatestVersion[Aout])
  val aCalc: () => Aout
  val bCalc: Aout => Bout
  val cCalc: Aout => Cout
  val dCalc: (Aout, Bout, Cout) => Dout

  val eCalc: (Eout, Dout) => Eout // recursive

  /**
    * How far can we get with just Function1? Looks pretty decent.
    */
  def pipelinePureFunction: Dout = {
    val aval = aCalc()
    dCalc(aval, bCalc(aval), cCalc(aval))
  }

  /**
    * Ideally, running a simulation is trivial: probably just
    *   for every day, getLatestValid for inputs, substitute a dummy
    *   for the part you don't want, and recalculate.
    *   We can probably even run it in parallel!
    *
    * Of course, you need to make sure to point Prod and Simulated data at different
    *   data stores.
    */
  object Simulation {
    val pipeline: Version => Dout = { version =>
      val ahydrated = aHydrate(version)
      dCalc(ahydrated, bCalc(ahydrated), cCalc(ahydrated))
    }
    val simulationResults: Iterable[Dout] =
      (1 to 10).par.map(date =>
        pipeline(getLatestVbyDate[Aout](date))
      ).seq
  }

  /**
    * What if we have a simulation that's self-referential and depends on the previous run?
    *
    * ...This is probably another good case for rigorous state management/accumulation
    */
  object RecursiveRunOverRunSimulation {
    val seedOutputE: Eout = new Eout {} // hydrated, dummy, manual seed in DB, who knows


    val recursivePipe: (Version, Eout) => Eout = { case (version, eout) =>
      val ahydrated = aHydrate(version)
      eCalc(eout, dCalc(ahydrated, bCalc(ahydrated), cCalc(ahydrated)))
    }

    val simulationResults: Iterable[Eout] =
      (1 to 10).foldLeft(Seq(seedOutputE)) { case (history, date) =>
        history :+ recursivePipe(getLatestVbyDate[Eout](date), history.last)
      }
  }

  /**
    * But what if we want B and C to run in parallel?
    * Can't accomplish this with just Functions... we'd need Futures (or Task/IO) for that.
    */
  def pipelineWrappedFutures: Dout = {
    import scala.concurrent.Future
    import scala.concurrent.Await
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    val af = Future(aCalc())

    val bf: Future[Bout] = for {
      aval <- af
    } yield bCalc(aval)

    val cf: Future[Cout] = for {
      aval <- af
    } yield cCalc(aval)

    val df = for {
      aval <- af
      bval <- bf
      cval <- cf
    } yield dCalc(aval, bval, cval)

    Await.result(df, 5.seconds)
  }

  /**
    * You could also re-write pipeline steps themselves to return Futures,
    *   if they internally had some par/async stuff going on.
    */
  def pipelineFuturizedCalcs: Dout = {
    import scala.concurrent.Future
    import scala.concurrent.Await
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    val aFut: () => Future[Aout] = ??? // implies default of "get latest valid"
    val bFut: (Aout) => Future[Bout] = ???
    val cFut: (Aout) => Future[Cout] = ???
    val dFut: (Aout, Bout, Cout) => Future[Dout] = ???

    val af: Future[Aout] = aFut()

    val bf: Future[Bout] = for {
      aval <- af
      bval <- bFut(aval)
    } yield bval

    val cf: Future[Cout] = for {
      aval <- af
      cval <- cFut(aval)
    } yield cval

    val df = for {
      aval <- af
      bval <- bf
      cval <- cf
      dval <- dFut(aval, bval, cval)
    } yield dval

    Await.result(df, 5.seconds)
  }

  /**
    * This is a tangent I'll hold off on, but instead of writing all app logic in terms of Future
    *   you could write it in terms of F : Monad; this way you can seamlessly switch between
    *   Future and IO at a later date if necessary,
    *   or use the Id monad (which is sync instead of async) for testing.
    */
  object PipeLineTagless {
    // TODO

    import cats.Monad
    def pipelineTaglessFinal[F[_]: Monad]: F[Dout] = ???
  }

  /**
    * This is another tangent (removed implementation):
    *   Given a Calc[I, O] and a Calc[Unit, I], I should be able to prove
    *   that I have a Calc[Unit, O], and etc, chaining together the pipeline.
    *
    * Theoretically, this might be useful if you had a library of Calcs,
    *   and often were building hierarchies.
    * Practically, it's probably not that expensive to write out manually,
    *   especially because if you're using the intermediate calcs you're already declaring them.
    *   It also comes with a pretty huge cost in readability/understanding,
    *   not to even mention tech complexity.
    *
    * Of course, this relies on strict types (you can achieve easily using tagged types)
    */
  object ImplicitChaining {

    // TODO finishing this requires probably some fancy HList shenanigans...
  }

  /**
    * Under any of these current schemes, it's easy to see ways of declaring inconsistent pipes:
    *   i.e. using two different versions of type A.
    *   There's a world in which this is actually intended;
    *   e.g. for testing/simulation you want to pass a simpler version of the data to one piece of the pipeline.
    */
  object AllowInconsistentVersions {
    val aval = aHydrate(VersionN(3))
    dCalc(aHydrate(VersionN(3)), bCalc(aval), cCalc(aval))
  }

  /**
    * But of course there's a world in which this is terrible, and we want to throw errors.
    *
    * Some we could catch an compile-time if we e.g. notice `getLatest` and `getByVersion`
    *   for the same type.
    * We could also use fancy `literal types` to make sure that all versions of a particular type
    *   are the same.
    *
    * Here's a nasty case though: what if values X and Y are both hydrated via `getLatest`,
    *   but usually X is used to compute Y. Production had an issue, and so the latest valid
    *   version of Y was built with a non-latest-valid version of X.
    *   TODO not really yet sure how to solve that...maybe a runtime check loading all of Y's dependencies?
    */
  object FailOnInconsistent {

    // TODO would like this to fail compilation, but need literal types + more structure than Function1
    val aval = VersionN(3) // should also fail for aHydrateLatest()
    dCalc(aHydrate(VersionN(3)), bCalc(aval), cCalc(aval))
  }

  //===================================================================================================================
  // Chapter 2: state management and accumulation

}
