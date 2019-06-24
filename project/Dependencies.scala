import sbt._
import Keys._
import sbt.contraband.ContrabandPlugin.autoImport._

object Dependencies {
  val scala212 = "2.12.8"

  def nightlyVersion: Option[String] = sys.props.get("sbt.build.version")

  private val ioVersion = nightlyVersion.getOrElse("1.3.0-M11")
  private val sbtIO = "org.scala-sbt" %% "io" % ioVersion

  def getSbtModulePath(key: String, name: String) = {
    val localProps = new java.util.Properties()
    IO.load(localProps, file("project/local.properties"))
    val path = Option(localProps getProperty key) orElse (sys.props get key)
    path foreach (f => println(s"Using $name from $f"))
    path
  }

  lazy val sbtIoPath = getSbtModulePath("sbtio.path", "sbt/io")

  def addSbtModule(p: Project,
                   path: Option[String],
                   projectName: String,
                   m: ModuleID,
                   c: Option[Configuration] = None) =
    path match {
      case Some(f) =>
        p dependsOn c.fold[ClasspathDep[ProjectReference]](ProjectRef(file(f), projectName))(
          ProjectRef(file(f), projectName) % _)
      case None => p settings (libraryDependencies += c.fold(m)(m % _))
    }

  def addSbtIO(p: Project): Project = addSbtModule(p, sbtIoPath, "io", sbtIO)

  val jline = "jline" % "jline" % "2.14.6"

  val scalaCompiler = Def.setting { "org.scala-lang" % "scala-compiler" % scalaVersion.value }
  val scalaReflect = Def.setting { "org.scala-lang" % "scala-reflect" % scalaVersion.value }

  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
  val scalaTest  = "org.scalatest"  %% "scalatest"  % "3.0.8" % Test
  val parserCombinator = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

  val sjsonnew = Def.setting {
    "com.eed3si9n" %% "sjson-new-core" % contrabandSjsonNewVersion.value
  }
  val sjsonnewScalaJson = Def.setting {
    "com.eed3si9n" %% "sjson-new-scalajson" % contrabandSjsonNewVersion.value
  }
  val sjsonnewMurmurhash = Def.setting {
    "com.eed3si9n" %% "sjson-new-murmurhash" % contrabandSjsonNewVersion.value
  }

  def log4jVersion = "2.11.2"
  val log4jApi = "org.apache.logging.log4j" % "log4j-api" % log4jVersion
  val log4jCore = "org.apache.logging.log4j" % "log4j-core" % log4jVersion
  val disruptor = "com.lmax" % "disruptor" % "3.4.2"
  val silencerPlugin = "com.github.ghik" %% "silencer-plugin" % "1.4.1"
  val silencerLib = "com.github.ghik" %% "silencer-lib" % "1.4.1" % Provided
}
