//> using scala 3.7
//> using platform jvm

// dependencies
//> using dep com.github.j-mie6::parsley::5.0.0-M17
//> using dep com.lihaoyi::os-lib::0.11.6
//> using test.dep org.scalatest::scalatest::3.2.19

// these are all sensible defaults to catch annoying issues
//> using options -deprecation -unchecked -feature
//> using options -Wimplausible-patterns -Wunused:all
//> using options -Yexplicit-nulls -Wsafe-init

// these are flags used by Scala native: if you aren't using scala-native, then they do nothing
// lto-thin has decent linking times, and release-fast does not too much optimisation.
// using nativeLto thin
// using nativeGc commix
// using nativeMode release-fast
