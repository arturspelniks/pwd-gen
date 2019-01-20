name := "pwd-gen"

version := "0.1"

scalaVersion := "2.12.8"
enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.6",
  "org.scalatest" %% "scalatest" % "3.0.5",
  "junit" % "junit" % "4.10" % Test
)