name := "shapes-oo-scala"

version := "0.4"

libraryDependencies ++= Seq(
  "org.creativescala" %% "doodle"     % "0.30.0",
  "org.scalatest"     %% "scalatest"  % "3.2.19"  % Test,
  "org.scalacheck"    %% "scalacheck" % "1.18.1"  % Test,
  
  // Added extra dependencies to support LazyLogging
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "ch.qos.logback"    %  "logback-classic" % "1.4.11"
)
