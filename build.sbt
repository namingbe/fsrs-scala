scalaVersion := "3.3.3"
organization := "com.namingbe"
name := "fsrs-scala"
version := "0.1.0"

Compile / managedSourceDirectories := Nil
Compile / scalaSource := baseDirectory.value / "src"
Compile / javaSource := baseDirectory.value / "src"  // if omitted, it'd add src/main/java to paths it checks

Compile / managedResourceDirectories := Nil
Compile / resourceDirectory := baseDirectory.value / "resources"

Test / managedSourceDirectories := Nil
Test / unmanagedSourceDirectories := Nil

idePackagePrefix := Some("com.namingbe")
