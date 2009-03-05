import sbt._

class DreadLocksProject(info: ProjectInfo) extends DefaultProject(info)
{
  // dependencies 
  val scalatest = "org.scalatest" % "scalatest" % "0.9.4"
  val compiler  = "org.scala-lang" % "scala-compiler" % "2.7.3"

  /// main
  override def mainClass = Some("org.scalalites.benchmark.RunBenchmark")
}
