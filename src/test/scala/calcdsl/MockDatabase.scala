package calcdsl

import java.util.concurrent.ConcurrentHashMap

import scala.util.Try


class MockDatabase[A](val name: RepositoryName,
                      preloaded: Seq[Versioned[A]] = Seq(),
                      timeoutMs: Long = 0) extends VersionedRepository[A]{

  var dbTable: ConcurrentHashMap[Version, A] = {
    val table = new ConcurrentHashMap[Version, A]()
    preloaded.foreach(v => table.put(v.version, v.get))
    table
  }

  override def read(version: Version): Try[Versioned[A]] = Try{
    Thread.sleep(timeoutMs)
    dbTable synchronized {
      Versioned(dbTable.get(version), version)
    }
  }

  override def write(fa: Versioned[A]): Try[Unit] = Try{
    Thread.sleep(timeoutMs)
    dbTable synchronized {
      require(! (dbTable contains fa.version), s"Tried to write version ${fa.version} but was already in mock DB.")
      dbTable.put(fa.version, fa.get)
    }
  }

}