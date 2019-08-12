package sbt.util.incremental

import java.io.File

import org.scalatest.{ Matchers, WordSpec }
import sbt.io.IO
import sbt.io.syntax._
import sbt.util.{ CacheStore, FileInfo }

class IncrementalSpec extends WordSpec with Matchers {

  implicit val style = FileInfo.hash

  "the runIncremental method" should {

    "always perform an op when there's no cache file" in {
      withStore { (cacheStore, tmpDir) =>
        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map[String, OpResult]("op1" -> OpFailure),
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "rerun an op when it failed last time" in {
      withStore { (cacheStore, tmpDir) =>
        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpFailure),
            prunedOps should ===(List("op1"))
          )
        }
        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpFailure),
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "skip an op if nothing's changed" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")
        IO.write(file1, "x")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpSuccess(filesRead = Set(file1), filesWritten = Set())),
            prunedOps should ===(List("op1"))
          )
        }

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map.empty,
            prunedOps shouldBe empty
          )
        }
      }
    }

    "rerun an op when the file it read has changed" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")
        IO.write(file1, "x")
        val file2 = new File(tmpDir, "2")
        IO.write(file1, "x")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpSuccess(filesRead = Set(file1), filesWritten = Set(file2))),
            prunedOps should ===(List("op1"))
          )
        }

        IO.write(file1, "y")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map.empty,
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "rerun an op when the file it wrote has changed" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          IO.write(file1, "x")
          (
            Map("op1" -> OpSuccess(filesRead = Set(), filesWritten = Set(file1))),
            prunedOps should ===(List("op1"))
          )
        }

        IO.write(file1, "y")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map.empty,
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "rerun an op when the file it wrote has been deleted" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          IO.write(file1, "x")
          (
            Map("op1" -> OpSuccess(filesRead = Set(), filesWritten = Set(file1))),
            prunedOps should ===(List("op1"))
          )
        }

        IO.delete(file1)

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map.empty,
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "rerun an op when some of the files it read have changed" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")
        val file2 = new File(tmpDir, "2")
        IO.write(file1, "x")
        IO.write(file2, "x")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpSuccess(filesRead = Set(file1, file2), filesWritten = Set())),
            prunedOps should ===(List("op1"))
          )
        }

        IO.write(file2, "y")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map.empty,
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "rerun an op when some of the files it wrote have changed" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")
        val file2 = new File(tmpDir, "2")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          IO.write(file1, "x")
          IO.write(file2, "x")
          (
            Map("op1" -> OpSuccess(filesRead = Set(), filesWritten = Set(file1, file2))),
            prunedOps should ===(List("op1"))
          )
        }

        IO.write(file2, "y")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map.empty,
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "run multiple ops" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")
        val file2 = new File(tmpDir, "2")
        val file3 = new File(tmpDir, "3")
        val file4 = new File(tmpDir, "4")
        IO.write(file1, "x")
        IO.write(file2, "x")
        IO.write(file3, "x")
        IO.write(file4, "x")

        syncIncremental(cacheStore, List("op1", "op2", "op3", "op4")) { prunedOps =>
          (
            Map(
              "op1" -> OpSuccess(filesRead = Set(file1), filesWritten = Set()),
              "op2" -> OpSuccess(filesRead = Set(file2), filesWritten = Set(file3)),
              "op3" -> OpSuccess(filesRead = Set(), filesWritten = Set(file4)),
              "op4" -> OpFailure
            ),
            prunedOps should ===(List("op1", "op2", "op3", "op4"))
          )
        }

        IO.write(file1, "y")
        IO.write(file4, "y")

        syncIncremental(cacheStore, List("op1", "op2", "op3", "op4")) { prunedOps =>
          (
            Map(
              "op1" -> OpSuccess(filesRead = Set(file1), filesWritten = Set()),
              "op3" -> OpSuccess(filesRead = Set(), filesWritten = Set(file4)),
              "op4" -> OpFailure
            ),
            prunedOps should ===(List("op1", "op3", "op4"))
          )
        }
      }
    }

    "vacuum unneeded ops from the cache" in {
      withStore { (cacheStore, tmpDir) =>
        // Create an empty cache
        syncIncremental(cacheStore, List[String]()) { prunedOps =>
          (
            Map.empty,
            prunedOps shouldBe empty
          )
        }

        val file1 = new File(tmpDir, "1")
        IO.write(file1, "x")

        // Run with a successful op that will be cached

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpSuccess(filesRead = Set(file1), filesWritten = Set())),
            prunedOps should ===(List("op1"))
          )
        }

        cacheStore.read[OpCache]().getContent should not be empty

        // Run with different set of ops - should vacuum old ops

        syncIncremental(cacheStore, List("op9")) { prunedOps =>
          (
            Map("op9" -> OpFailure),
            prunedOps should ===(List("op9"))
          )
        }

        // Check cache file is empty again, i.e. op1 has been vacuumed

        cacheStore.read[OpCache]().getContent shouldBe empty

      }
    }

    "rerun an op if its hash changes" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")
        val file2 = new File(tmpDir, "2")
        IO.write(file1, "x")
        IO.write(file2, "x")

        var hashPrefix = ""
        implicit val hasher = OpInputHasher[String](op => OpInputHash.hashString(hashPrefix + op))

        // Cache ops with an initial hash prefix

        hashPrefix = "1/"

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpSuccess(filesRead = Set(file1), filesWritten = Set(file2))),
            prunedOps should ===(List("op1"))
          )
        }

        // No ops should run because we leave the hash prefix the same

        hashPrefix = "1/"

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map(),
            prunedOps shouldBe empty
          )
        }

        // All ops should run again because we changed the hash prefix

        hashPrefix = "2/"

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpSuccess(filesRead = Set(file1), filesWritten = Set(file2))),
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "fail when runOps gives result for unknown op" in {
      withStore { (cacheStore, tmpDir) =>
        an[IllegalArgumentException] should be thrownBy syncIncremental(cacheStore, List("op1")) {
          prunedOps =>
            (
              Map[String, OpResult]("op2" -> OpFailure),
              prunedOps should ===(List("op1"))
            )
        }
      }
    }

  }

  "the syncIncremental method" should {

    "always perform an op when there's no cache file" in {
      withStore { (cacheStore, tmpDir) =>
        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map[String, OpResult]("op1" -> OpFailure),
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "rerun an op when it failed last time" in {
      withStore { (cacheStore, tmpDir) =>
        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpFailure),
            prunedOps should ===(List("op1"))
          )
        }
        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpFailure),
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "skip an op if nothing's changed" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")
        IO.write(file1, "x")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpSuccess(filesRead = Set(file1), filesWritten = Set())),
            prunedOps should ===(List("op1"))
          )
        }
        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map.empty,
            prunedOps shouldBe empty
          )
        }
      }
    }

    "rerun an op when the file it read has changed" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")
        IO.write(file1, "x")
        val file2 = new File(tmpDir, "2")
        IO.write(file1, "x")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpSuccess(filesRead = Set(file1), filesWritten = Set(file2))),
            prunedOps should ===(List("op1"))
          )
        }

        IO.write(file1, "y")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map.empty,
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "rerun an op when the file it wrote has changed" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          IO.write(file1, "x")
          (
            Map("op1" -> OpSuccess(filesRead = Set(), filesWritten = Set(file1))),
            prunedOps should ===(List("op1"))
          )
        }

        IO.write(file1, "y")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map.empty,
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "rerun an op when the file it wrote has been deleted" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          IO.write(file1, "x")
          (
            Map("op1" -> OpSuccess(filesRead = Set(), filesWritten = Set(file1))),
            prunedOps should ===(List("op1"))
          )
        }

        IO.delete(file1)

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map.empty,
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "rerun an op when some of the files it read have changed" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")
        val file2 = new File(tmpDir, "2")
        IO.write(file1, "x")
        IO.write(file2, "x")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpSuccess(filesRead = Set(file1, file2), filesWritten = Set())),
            prunedOps should ===(List("op1"))
          )
        }

        IO.write(file2, "y")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map.empty,
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "rerun an op when some of the files it wrote have changed" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")
        val file2 = new File(tmpDir, "2")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          IO.write(file1, "x")
          IO.write(file2, "x")
          (
            Map("op1" -> OpSuccess(filesRead = Set(), filesWritten = Set(file1, file2))),
            prunedOps should ===(List("op1"))
          )
        }

        IO.write(file2, "y")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map.empty,
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "run multiple ops" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")
        val file2 = new File(tmpDir, "2")
        val file3 = new File(tmpDir, "3")
        val file4 = new File(tmpDir, "4")
        IO.write(file1, "x")
        IO.write(file2, "x")
        IO.write(file3, "x")
        IO.write(file4, "x")

        syncIncremental(cacheStore, List("op1", "op2", "op3", "op4")) { prunedOps =>
          (
            Map(
              "op1" -> OpSuccess(filesRead = Set(file1), filesWritten = Set()),
              "op2" -> OpSuccess(filesRead = Set(file2), filesWritten = Set(file3)),
              "op3" -> OpSuccess(filesRead = Set(), filesWritten = Set(file4)),
              "op4" -> OpFailure
            ),
            prunedOps should ===(List("op1", "op2", "op3", "op4"))
          )
        }

        IO.write(file1, "y")
        IO.write(file4, "y")

        syncIncremental(cacheStore, List("op1", "op2", "op3", "op4")) { prunedOps =>
          (
            Map(
              "op1" -> OpSuccess(filesRead = Set(file1), filesWritten = Set()),
              "op3" -> OpSuccess(filesRead = Set(), filesWritten = Set(file4)),
              "op4" -> OpFailure
            ),
            prunedOps should ===(List("op1", "op3", "op4"))
          )
        }
      }
    }

    "vacuum unneeded ops from the cache" in {
      withStore { (cacheStore, tmpDir) =>
        // Create an empty cache
        syncIncremental(cacheStore, List[String]()) { prunedOps =>
          (
            Map.empty,
            prunedOps shouldBe empty
          )
        }

        val file1 = new File(tmpDir, "1")
        IO.write(file1, "x")

        // Run with a successful op that will be cached

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpSuccess(filesRead = Set(file1), filesWritten = Set())),
            prunedOps should ===(List("op1"))
          )
        }

        cacheStore.read[OpCache]().getContent should not be empty

        // Run with different set of ops - should vacuum old ops

        syncIncremental(cacheStore, List("op9")) { prunedOps =>
          (
            Map("op9" -> OpFailure),
            prunedOps should ===(List("op9"))
          )
        }

        // Check cache file is empty again, i.e. op1 has been vacuumed
        cacheStore.read[OpCache]().getContent shouldBe empty

      }
    }

    "rerun an op if its hash changes" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")
        val file2 = new File(tmpDir, "2")
        IO.write(file1, "x")
        IO.write(file2, "x")

        var hashPrefix = ""
        implicit val hasher = OpInputHasher[String](op => OpInputHash.hashString(hashPrefix + op))

        // Cache ops with an initial hash prefix

        hashPrefix = "1/"

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpSuccess(filesRead = Set(file1), filesWritten = Set(file2))),
            prunedOps should ===(List("op1"))
          )
        }

        // No ops should run because we leave the hash prefix the same

        hashPrefix = "1/"

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map(),
            prunedOps shouldBe empty
          )
        }

        // All ops should run again because we changed the hash prefix

        hashPrefix = "2/"

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map("op1" -> OpSuccess(filesRead = Set(file1), filesWritten = Set(file2))),
            prunedOps should ===(List("op1"))
          )
        }
      }
    }

    "fail when runOps gives result for unknown op" in {
      withStore { (cacheStore, tmpDir) =>
        an[IllegalArgumentException] should be thrownBy syncIncremental(cacheStore, List("op1")) {
          prunedOps =>
            (
              Map[String, OpResult]("op2" -> OpFailure),
              prunedOps should ===(List("op1"))
            )
        }
      }
    }

    "delete a file if a previous op has been removed" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")
        val file2 = new File(tmpDir, "2")

        syncIncremental(cacheStore, List("op1", "op2")) { prunedOps =>
          IO.write(file1, "x")
          IO.write(file2, "x")
          (
            Map[String, OpResult](
              "op1" -> OpSuccess(Set.empty, Set(file1)),
              "op2" -> OpSuccess(Set.empty, Set(file2))
            ),
            ()
          )
        }
        val (outputFiles, _) = syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map.empty[String, OpResult],
            prunedOps shouldBe empty
          )
        }

        outputFiles should ===(Set(file1))

        file1 should exist
        file2 shouldNot exist
      }
    }

    "delete a file if it's no longer produced by an op" in {
      withStore { (cacheStore, tmpDir) =>
        val infile = new File(tmpDir, "in")
        val file1 = new File(tmpDir, "1")
        val file2 = new File(tmpDir, "2")

        IO.write(infile, "1")

        syncIncremental(cacheStore, List("op1")) { prunedOps =>
          IO.write(file1, "x")
          IO.write(file2, "x")
          (
            Map[String, OpResult](
              "op1" -> OpSuccess(Set(infile), Set(file1, file2))
            ),
            ()
          )
        }

        IO.write(infile, "2")

        val (outputFiles, _) = syncIncremental(cacheStore, List("op1")) { prunedOps =>
          (
            Map[String, OpResult](
              "op1" -> OpSuccess(Set(infile), Set(file1))
            ),
            prunedOps should ===(List("op1"))
          )
        }

        outputFiles should ===(Set(file1))

        file1 should exist
        file2 shouldNot exist
      }
    }

    "not delete a file if it's produced by another op" in {
      withStore { (cacheStore, tmpDir) =>
        val file1 = new File(tmpDir, "1")
        val file2 = new File(tmpDir, "2")
        val infile = new File(tmpDir, "in")

        syncIncremental(cacheStore, List("op1", "op2")) { prunedOps =>
          IO.write(file1, "x")
          IO.write(file2, "x")
          (
            Map[String, OpResult](
              "op1" -> OpSuccess(Set(infile), Set(file1)),
              "op2" -> OpSuccess(Set.empty, Set(file2))
            ),
            ()
          )
        }

        IO.write(infile, "2")

        val (outputFiles, _) = syncIncremental(cacheStore, List("op1", "op3")) { prunedOps =>
          (
            Map[String, OpResult](
              "op1" -> OpSuccess(Set(infile), Set.empty),
              "op3" -> OpSuccess(Set.empty, Set(file1, file2))
            ),
            ()
          )
        }

        outputFiles should ===(Set(file1, file2))

        file1 should exist
        file2 should exist
      }
    }

  }

  private def withStore(f: (CacheStore, File) => Any): Unit = {
    val _ = IO.withTemporaryDirectory { tmp =>
      val store = CacheStore(tmp / "cache-store")
      f(store, tmp)
    }
  }
}
