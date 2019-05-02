package calcs.calcasfunction.smartdomain

import scala.util.{Success, Try}


trait Binder[T] {

  def get: T

  /**
    * Pushes a new CalcRun context onto T
    *   so that T knows in what context it's in.
    */
  def bind(calcRun: CalcRun): Binder[T]

  /**
    * Returns a new T with the context unbinded.
    */
  //  def unbind: Binder[T]
}

/**
  * TODO transition APIs to Future, then Try, and finally abstract over F[_] (tagless final)
  */

// TODO use macro or classpath to eliminate collisions
case class StorageTypeRepresentation(name: String)
//  final def fullName: String = s"$dataConceptName: ${this.getClass.getName}"

trait Repo[T] extends Binder[T] {
  def name: StorageTypeRepresentation
}

trait Bundle[T <: Binder[T]] extends Binder[T] {
  this: T =>
  override final def get: T = this
  def get(repo: Repo[T]): T
}

trait Hydrator[T]{
  def hydrate(version: CalcRun): Try[T]
  def hydrateLatestValid(): Try[Versioned[T]]
}

trait Persister[T]{
  def dbTypeRepr: StorageTypeRepresentation

  protected def persist(t: VersionedUnpersisted[T]): Try[Unit]

  final def persistWrap(t: VersionedUnpersisted[T]): Try[Versioned[T]] = {
    for (_ <- this.persist(t)) yield t.toPersisted
  }
}


// =========================================================================================================
// Implementations
// =========================================================================================================

trait BundleImpl[T <: Bundle[T]] extends Bundle[T] {
  outer: T =>

  def metadataRepository: MetadataRepository
  val contextStack: ImmutableStack[CalcRun] = new ImmutableStack[CalcRun]

  // TODO(nick.bradford) need to force users to define a copy constructor for all Binder[T]s ?
  override final def bind(calcRun: CalcRun): BundleImpl[T] = {
    new BundleImpl[T] {
      override val contextStack: ImmutableStack[CalcRun] = outer.contextStack.push(calcRun)
      override val metadataRepository: MetadataRepository = outer.metadataRepository
    }
  }
  override def get(repo: Repo[T]) = repo.get // TODO a roundabout way to enforce that everyone logs their data
}


trait RepoContextImpl[T] extends Repo[T] {

  outer =>

  protected def innerGet: T
  def version: CalcRun
  def metadataRepository: MetadataRepository
  val contextStack: ImmutableStack[CalcRun] = new ImmutableStack[CalcRun]

  // TODO this would be a copy constructor if RepoImpl was a case class...
  override final def bind(calcRun: CalcRun): RepoContextImpl[T] = {
    new RepoContextImpl[T] {
      override val contextStack: ImmutableStack[CalcRun] = outer.contextStack.push(calcRun)
      override val metadataRepository: MetadataRepository = outer.metadataRepository
      override def name: StorageTypeRepresentation = outer.name
      override def version: CalcRun = outer.version
      protected def innerGet: T = outer.innerGet
    }
  }

  // should be Try
  override final def get: T = {
    val persistMetadata: Try[Unit] = contextStack.headOption match {
      case Some(context) =>
        val record = InputRecord(context, this.version, this.name)
        metadataRepository.logInput(record)
      case None => Success()
    }
    innerGet
  }

}

// wrapper for better semantics around constant-time stack-like operations with List
class ImmutableStack[T](inner: List[T] = Nil) {
  def push(t: T): ImmutableStack[T] = new ImmutableStack(t +: inner)
  def pop: ImmutableStack[T] = new ImmutableStack(inner.tail)
  def headOption: Option[T] = inner.headOption
}


