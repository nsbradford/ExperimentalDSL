package calcs.calcasfunction.smartdomain

import scala.util.{Success, Try}


trait BundleImpl[T <: Bundle[T]] extends Bundle[T] {
  outer: T =>

  def metadataRepository: MetadataRepository
  val contextStack: ImmutableStack[CalcRun] = new ImmutableStack[CalcRun]

  // TODO(nick.bradford) need to force users to define a copy constructor for all Binder[T]s ?
  // TODO need some way to get bind() to also bind() all the inner Repos...
  override final def bind(calcRun: CalcRun): T = // or does this need to be a Bundle[T]?
  { ???
//    new BundleImpl[T] {
//      override val contextStack: ImmutableStack[CalcRun] = outer.contextStack.push(calcRun)
//      override val metadataRepository: MetadataRepository = outer.metadataRepository
//    }
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
//    new RepoContextImpl[T] {
//      override val contextStack: ImmutableStack[CalcRun] = outer.contextStack.push(calcRun)
//      override val metadataRepository: MetadataRepository = outer.metadataRepository
//      override def name: StorageTypeRepresentation = outer.name
//      override def version: CalcRun = outer.version
//      protected def innerGet: T = outer.innerGet
//    }
    ???
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


