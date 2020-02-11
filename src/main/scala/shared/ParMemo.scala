package shared


import com.google.common.cache._

import scala.util.{Failure, Success, Try}

/**
  * Created by nicholasbradford on 2/10/20.
  */
class ParMemo[K, V] private (f: K => V) extends (K => V) {

  private val loadingCache: LoadingCache[AnyRef, AnyRef] = CacheBuilder.newBuilder()
    .build(
      new CacheLoader[AnyRef, AnyRef] {
        override def load(key: AnyRef): AnyRef = {
          f(key.asInstanceOf[K]).asInstanceOf[AnyRef]
        }
      }
    )

  override def apply(key: K): V = {
    Try{
      loadingCache.get(key.asInstanceOf[AnyRef]).asInstanceOf[V]
    } match {
      case Success(v) => v
      case Failure(e @(
        _: com.google.common.util.concurrent.UncheckedExecutionException
        | _: com.google.common.util.concurrent.ExecutionError
        | _: java.util.concurrent.ExecutionException
        )) => throw e.getCause
      case Failure(e) => throw e
    }
  }
}

object ParMemo{
  def apply[K, V](f: K => V): K => V = new ParMemo(f)
}
