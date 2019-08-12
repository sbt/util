/*
 * Copyright (C) 2009-2019 Lightbend Inc. <https://www.lightbend.com>
 */
package sbt.util.incremental

import java.io.File

import sbt.util.{ FileInfo, FilesInfo }
import sbt.util.FileInfo.Style
import sjsonnew.{ Builder, JsonFormat, JsonReader, JsonWriter, Unbuilder, deserializationError }

import scala.collection.immutable.Set

/**
 * Cache for recording which operations have successfully completed. Associates
 * a hash of the operations' inputs (OpInputHash) with a record of the files
 * that were accessed by the operation.
 */
private[incremental] final class OpCache(val style: Style) {
  type F = style.F

  private var content: Map[OpInputHash, Record] = Map.empty

  def allOpInputHashes: Set[OpInputHash] = content.keySet
  def contains(oih: OpInputHash): Boolean = {
    content.contains(oih)
  }
  def getRecord(oih: OpInputHash): Option[Record] = {
    content.get(oih)
  }
  def putRecord(oih: OpInputHash, record: Record): Unit = {
    content = content + ((oih, record))
  }
  def removeRecord(oih: OpInputHash): Unit = {
    content = content - oih
  }
  def getContent: Map[OpInputHash, Record] = content

  /**
   * A record stored in a cache. At the moment a record stores
   * a list of files accessed, but the record could be extended to store other
   * information in the future.
   */
  case class Record(filesInfo: FilesInfo[style.F], products: Set[File])
}

/**
 * Useful methods for working with an OpCache.
 */
private[incremental] object OpCache {

  /**
   * Check if any of the given FileInfo objects have changed.
   */
  def anyFileChanged(fileInfos: Set[_ <: FileInfo], style: Style): Boolean = {
    fileInfos.exists { fileInfo =>
      style.apply(fileInfo.file) != fileInfo
    }
  }

  /**
   * Remove all operations from the cache that aren't in the given set of operations.
   */
  def vacuumExcept[Op](cache: OpCache, opsToKeep: Seq[Op])(
      implicit opInputHasher: OpInputHasher[Op]
  ): Unit = {
    val oihSet: Set[OpInputHash] = opsToKeep.map(opInputHasher.hash).toSet

    cache.allOpInputHashes
      .filterNot(oihSet)
      .foreach(cache.removeRecord)
  }

  /**
   * Given a set of operations, filter out any operations that are in the cache
   * and unchanged, and only return operations that are not in the cache or that
   * are in the cache but have changed.
   */
  def newOrChanged[Op](cache: OpCache, ops: Seq[Op])(
      implicit opInputHasher: OpInputHasher[Op]
  ): Seq[Op] = {
    val opsAndHashes: Seq[(Op, OpInputHash)] = ops.map(w => (w, opInputHasher.hash(w)))
    opsAndHashes.filter {
      case (_, wh) =>
        cache.getRecord(wh).fold(true) { record =>
          // Check that cached file hashes are up to date
          val fileChanged = OpCache.anyFileChanged(record.filesInfo.files, cache.style)
          if (fileChanged) cache.removeRecord(wh)
          fileChanged
        }
    } map {
      case (w, _) => w
    }
  }

  /**
   * Add the result of an operation into the cache.
   */
  def cacheResult(cache: OpCache, oih: OpInputHash, or: OpResult): Unit = or match {
    case OpFailure =>
      // Shouldn't actually be present in the cache, but clear just in case
      if (cache.contains(oih)) cache.removeRecord(oih)
    case OpSuccess(filesRead, filesWritten) =>
      val fileHashes: FilesInfo[cache.style.F] = cache.style.apply(filesRead ++ filesWritten)
      val record = cache.Record(fileHashes, filesWritten)
      cache.putRecord(oih, record)
  }

  /**
   * Add multiple operations and results into the cache.
   */
  def cacheResults[Op](cache: OpCache, results: Map[Op, OpResult])(
      implicit opInputHasher: OpInputHasher[Op]
  ): Unit = {
    for ((op, or) <- results) {
      cacheResult(cache, opInputHasher.hash(op), or)
    }
  }

  /**
   * Get all the products for the given ops in the cache.
   */
  def productsForOps[Op](cache: OpCache, ops: Set[Op])(
      implicit opInputHasher: OpInputHasher[Op]
  ): Set[File] = {
    ops.flatMap { op =>
      val record = cache.getRecord(opInputHasher.hash(op))
      record.fold(Set.empty[File])(_.products)
    }
  }

  implicit def jsonFormat(implicit style: Style): JsonFormat[OpCache] = {
    import sjsonnew.BasicJsonProtocol._

    new JsonFormat[OpCache] {
      override def write[J](cache: OpCache, builder: Builder[J]): Unit = {

        import cache.style.formats

        implicit val recordWriter: JsonWriter[cache.Record] = new JsonWriter[cache.Record] {
          override def write[J1](record: cache.Record, builder: Builder[J1]): Unit = {
            builder.beginObject()
            builder.addField("filesInfo", record.filesInfo)
            builder.addField("products", record.products.map(_.getAbsolutePath))
            builder.endObject()
          }
        }

        builder.beginObject()
        cache.content.foreach {
          case (hash, record) =>
            builder.addField(hash.hash, record)
        }
        builder.endObject()
      }

      override def read[J](jsOpt: Option[J], unbuilder: Unbuilder[J]): OpCache = {
        val cache = new OpCache(style)
        import cache.style.formats

        val recordReader = new JsonReader[cache.Record] {
          override def read[J1](jsOpt: Option[J1], unbuilder: Unbuilder[J1]): cache.Record = {
            jsOpt match {
              case Some(js) =>
                unbuilder.beginObject(js)
                val filesInfo = unbuilder.readField[FilesInfo[cache.F]]("filesInfo")
                val products = unbuilder.readField[Set[String]]("products")
                unbuilder.endObject()
                cache.Record(filesInfo, products.map(new File(_)))
              case None => deserializationError("Expected JsObject but found None")
            }
          }
        }

        jsOpt match {
          case Some(js) =>
            unbuilder.beginObject(js)
            while (unbuilder.hasNextField) {
              val (hash, value) = unbuilder.nextFieldOpt()
              val record = recordReader.read(value, unbuilder)
              cache.putRecord(OpInputHash(hash), record)
            }
            unbuilder.endObject()
            cache
          case None => deserializationError("Expected JsObject but found None")
        }
      }
    }

  }
}
