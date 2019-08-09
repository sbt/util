package sbt.util

import org.scalatest.FlatSpec
import sbt.io.IO
import sbt.io.syntax._
import sbt.util.CacheImplicits._

import scala.concurrent.Promise
import scala.util.Try

class TrackedSpec extends FlatSpec {
  "lastOutput" should "store the last output" in {
    withStore { store =>
      val value = 5
      val otherValue = 10

      val res0 =
        Tracked.lastOutput[Int, Int](store) {
          case (in, None) =>
            assert(in === value)
            in
          case (_, Some(_)) =>
            fail()
        }(implicitly)(value)
      assert(res0 === value)

      val res1 =
        Tracked.lastOutput[Int, Int](store) {
          case (_, None) =>
            fail()
          case (in, Some(read)) =>
            assert(in === otherValue)
            assert(read === value)
            read
        }(implicitly)(otherValue)
      assert(res1 === value)

      val res2 =
        Tracked.lastOutput[Int, Int](store) {
          case (_, None) =>
            fail()
          case (in, Some(read)) =>
            assert(in === otherValue)
            assert(read === value)
            read
        }(implicitly)(otherValue)
      assert(res2 === value)
    }
  }

  "inputChanged" should "detect that the input has not changed" in {
    withStore { store =>
      val input0 = "foo"

      val res0 =
        Tracked.inputChanged[String, String](store) {
          case (true, in) =>
            assert(in === input0)
            in
          case (false, _) =>
            fail()
        }(implicitly, implicitly)(input0)
      assert(res0 === input0)

      val res1 =
        Tracked.inputChanged[String, String](store) {
          case (true, _) =>
            fail()
          case (false, in) =>
            assert(in === input0)
            in
        }(implicitly, implicitly)(input0)
      assert(res1 === input0)
    }
  }

  it should "detect that the input has changed" in {
    withStore { store =>
      val input0 = 0
      val input1 = 1

      val res0 =
        Tracked.inputChanged[Int, Int](store) {
          case (true, in) =>
            assert(in === input0)
            in
          case (false, _) =>
            fail()
        }(implicitly, implicitly)(input0)
      assert(res0 === input0)

      val res1 =
        Tracked.inputChanged[Int, Int](store) {
          case (true, in) =>
            assert(in === input1)
            in
          case (false, _) =>
            fail()
        }(implicitly, implicitly)(input1)
      assert(res1 === input1)
    }
  }

  "outputChanged" should "detect that the output has not changed" in {
    withStore { store =>
      val beforeCompletion: String = "before-completion"
      val afterCompletion: String = "after-completion"
      val sideEffectCompleted = Promise[Unit]
      val p0: () => String = () => {
        if (sideEffectCompleted.isCompleted) {
          afterCompletion
        } else {
          sideEffectCompleted.success(())
          beforeCompletion
        }
      }
      val firstExpectedResult = "first-result"
      val secondExpectedResult = "second-result"

      val res0 =
        Tracked.outputChanged[String, String](store) {
          case (true, in) =>
            assert(in === beforeCompletion)
            firstExpectedResult
          case (false, _) =>
            fail()
        }(implicitly)(p0)
      assert(res0 === firstExpectedResult)

      val res1 =
        Tracked.outputChanged[String, String](store) {
          case (true, _) =>
            fail()
          case (false, in) =>
            assert(in === afterCompletion)
            secondExpectedResult
        }(implicitly)(p0)
      assert(res1 === secondExpectedResult)
    }
  }

  "tstamp tracker" should "have a timestamp of 0 on first invocation" in {
    withStore { store =>
      Tracked.tstamp(store) { last =>
        assert(last === 0)
      }
    }
  }

  it should "provide the last time a function has been evaluated" in {
    withStore { store =>
      Tracked.tstamp(store) { last =>
        assert(last === 0)
      }

      Tracked.tstamp(store) { last =>
        val difference = System.currentTimeMillis - last
        assert(difference < 1000)
      }
    }
  }

  "Difference.outputs" should "commit the cache will a snapshot of all input files after the execution" in {
    withStore { store =>
      val outCache = Difference.outputs(store, FileInfo.lastModified)
      IO.withTemporaryDirectory { tmp =>
        val files = Seq(tmp / "file0", tmp / "file1", tmp / "file2").map(_.getAbsoluteFile)

        outCache(files.toSet)(_ => IO.touch(files(1)))
        Thread.sleep(10)

        IO.touch(files(2))
        Thread.sleep(10)

        val newInput = (tmp / "newInput").getAbsoluteFile
        outCache(files.toSet + newInput) { report =>
          assert(report.checked == files.toSet + newInput)
          assert(report.unmodified == files.toSet - files(2))
          assert(report.modified == Set(newInput, files(2)))
          assert(report.added == Set(newInput))
          assert(report.removed == Set())
        }
      }
    }
  }

  "Difference.outputs" should "allow to selectively update the cache after the execution" in {
    withStore { store =>
      val outCache = Difference.outputs(store, FileInfo.lastModified)
      IO.withTemporaryDirectory { tmp =>
        val files = Seq(tmp / "file0", tmp / "file1", tmp / "file2").map(_.getAbsoluteFile)

        val toCache: PartialFunction[Seq[File], Set[File]] = {
          case output => (output.toSet - files(2)) // cache only files(1)
        }
        outCache[Seq[File]](files.toSet, toCache) { _ =>
          IO.touch(files(1))
          Thread.sleep(10)
          Seq(files(1), files(2))
        }

        val newInput = (tmp / "newInput").getAbsoluteFile
        outCache(
          files.toSet + newInput,
          PartialFunction.empty[Any, Set[File]] // do NOT update cache
        ) { report =>
          assert(report.added.contains(files(2)))
          assert(report.unmodified == Set(files(1))) // the cache was updated AFTER the previous execution
          IO.touch(files(1))
          Thread.sleep(10)
        }

        outCache(files.toSet + newInput) { report =>
          assert(report.added.contains(newInput)) // previous execution did NOT cache it
          assert(report.modified.contains(files(1))) // last update was not captured
        }
      }
    }
  }

  "Difference.inputs" should "succeed without a custom cache-update-controlling PartialFunction" in {
    withStore { store =>
      val outCache = Difference.inputs(store, FileInfo.lastModified)
      outCache(Set.empty[File])(_ => true)
    }
  }

  "Difference.inputs" should "fail verbosely with a custom cache-update-controlling PartialFunction" in {
    withStore { store =>
      val outCache = Difference.inputs(store, FileInfo.lastModified)
      assert(Try(outCache(Set.empty[File], PartialFunction.empty)(_ => true)).isFailure)
    }
  }

  private def withStore[T](f: CacheStore => T): T =
    IO.withTemporaryDirectory { tmp =>
      val store = CacheStore(tmp / "cache-store")
      f(store)
    }

}
