/*
 * Copyright (C) 2009-2019 Lightbend Inc. <https://www.lightbend.com>
 */
package sbt.util.incremental

import java.io.File
import scala.collection.immutable.Set

/**
 * The result of running an operation, either OpSuccess or OpFailure.
 */
sealed trait OpResult

/**
 * An operation that succeeded. Contains information about which files the
 * operation read and wrote.
 */
final case class OpSuccess(filesRead: Set[File], filesWritten: Set[File]) extends OpResult

/**
 * An operation that failed.
 */
case object OpFailure extends OpResult
