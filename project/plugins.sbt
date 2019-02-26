resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
// The Play plugin
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.3")
// addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")、
addSbtPlugin("com.heroku" % "sbt-heroku" % "2.1.2")
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.18")
