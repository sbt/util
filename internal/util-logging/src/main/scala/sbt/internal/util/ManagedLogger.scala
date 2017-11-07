package sbt.internal.util

import sbt.util.{ Level, _ }
import org.apache.logging.log4j.{ Logger => XLogger }
import org.apache.logging.log4j.message.ObjectMessage
import sjsonnew.JsonFormat

import scala.reflect.runtime.universe.TypeTag
import sbt.internal.util.codec.JsonProtocol._

/**
 * Delegates log events to the associated LogExchange.
 */
class ManagedLogger(
    val name: String,
    val channelName: Option[String],
    val execId: Option[String],
    xlogger: XLogger
) extends Logger {
  override def trace(t: => Throwable): Unit =
    logEvent(Level.Error, TraceEvent("Error", t, channelName, execId))
  override def log(level: Level.Value, message: => String): Unit = {
    if (shouldLog(level)) {
      xlogger.log(
        ConsoleAppender.toXLevel(level),
        new ObjectMessage(StringEvent(level.toString, message, channelName, execId))
      )
    }
  }

  // send special event for success since it's not a real log level
  override def success(message: => String): Unit = {
    infoEvent[SuccessEvent](SuccessEvent(message))
  }

  def registerStringCodec[A: ShowLines: TypeTag]: Unit = {
    LogExchange.registerStringCodec[A]
  }

  final def debugEvent[A: JsonFormat: TypeTag](event: => A): Unit = logEvent(Level.Debug, event)
  final def infoEvent[A: JsonFormat: TypeTag](event: => A): Unit = logEvent(Level.Info, event)
  final def warnEvent[A: JsonFormat: TypeTag](event: => A): Unit = logEvent(Level.Warn, event)
  final def errorEvent[A: JsonFormat: TypeTag](event: => A): Unit = logEvent(Level.Error, event)

  def logEvent[A: JsonFormat: TypeTag](level: Level.Value, event: => A): Unit = {
    if (shouldLog(level)) {
      val v: A = event
      val tag = StringTypeTag[A]
      LogExchange.getOrElseUpdateJsonCodec(tag.key, implicitly[JsonFormat[A]])
      // println("logEvent " + tag.key)
      val entry: ObjectEvent[A] = ObjectEvent(level, v, channelName, execId, tag.key)
      xlogger.log(
        ConsoleAppender.toXLevel(level),
        new ObjectMessage(entry)
      )
    }
  }

  private def shouldLog(level: Level.Value) =
    xlogger.getLevel.isMoreSpecificThan(ConsoleAppender.toXLevel(level))

  @deprecated("No longer used.", "1.0.0")
  override def ansiCodesSupported = ConsoleAppender.formatEnabledInEnv
}
