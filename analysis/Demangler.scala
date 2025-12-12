package analysis

import process.*

/** C++ symbol demangler using c++filt */
object Demangler:

  private var cxxfiltAvailable: Option[Boolean] = None
  private val cache = scala.collection.mutable.Map.empty[String, Option[String]]

  /** Check if c++filt is available on the system */
  def isAvailable(runner: ProcessRunner): Boolean =
    cxxfiltAvailable.getOrElse {
      val available = runner.run("which", "c++filt").isSuccess
      cxxfiltAvailable = Some(available)
      available
    }

  /** Check if a symbol looks like a C++ mangled name */
  def isMangledCpp(name: String): Boolean =
    // Itanium ABI: starts with _Z (Linux) or __Z (macOS)
    // Also handle _ZN, _ZSt, etc.
    name.startsWith("_Z") || name.startsWith("__Z")

  /** Demangle a single C++ symbol.
    * Returns Some(demangled) if successful, None if not a C++ symbol or demangling failed.
    */
  def demangle(name: String, runner: ProcessRunner): Option[String] =
    if !isMangledCpp(name) then None
    else
      cache.getOrElseUpdate(name, demangleUncached(name, runner))

  private def demangleUncached(name: String, runner: ProcessRunner): Option[String] =
    if !isAvailable(runner) then None
    else
      // Try demangling the symbol as-is first
      tryDemangle(name, runner).orElse {
        // On macOS, symbols have an extra underscore prefix
        // If _Z... fails, try with __Z... (and vice versa)
        if name.startsWith("__Z") then
          tryDemangle(name.stripPrefix("_"), runner)
        else if name.startsWith("_Z") then
          tryDemangle("_" + name, runner)
        else
          None
      }

  private def tryDemangle(name: String, runner: ProcessRunner): Option[String] =
    runner.run("c++filt", name) match
      case ProcessResult.Success(stdout, _) =>
        val demangled = stdout.trim
        // c++filt returns the original if it can't demangle
        if demangled != name && !demangled.startsWith("_Z") && !demangled.startsWith("__Z") && demangled.nonEmpty then 
          Some(demangled)
        else None
      case _ => None

  /** Demangle multiple symbols in batch */
  def demangleBatch(names: Seq[String], runner: ProcessRunner): Map[String, String] =
    if !isAvailable(runner) then return Map.empty

    val mangledNames = names.filter(isMangledCpp).distinct
    if mangledNames.isEmpty then return Map.empty

    // Check cache first
    val (cached, uncached) = mangledNames.partition(cache.contains)
    val result = scala.collection.mutable.Map.empty[String, String]

    // Add cached results
    for name <- cached do
      cache(name).foreach(demangled => result(name) = demangled)

    // Demangle uncached symbols individually
    // (ProcessRunner doesn't support stdin, so we can't batch via c++filt's stdin)
    for name <- uncached do
      demangleUncached(name, runner).foreach { demangled =>
        cache(name) = Some(demangled)
        result(name) = demangled
      }

    result.toMap

  /** Format a symbol name with optional demangled version in parentheses */
  def formatSymbol(name: String, runner: ProcessRunner): String =
    demangle(name, runner) match
      case Some(demangled) => s"$name ($demangled)"
      case None            => name

  /** Format a symbol name with optional demangled version, using cached demangle map */
  def formatSymbolCached(name: String, demangleMap: Map[String, String]): String =
    demangleMap.get(name) match
      case Some(demangled) => s"$name ($demangled)"
      case None            => name

  /** Clear the demangling cache */
  def clearCache(): Unit =
    cache.clear()
    cxxfiltAvailable = None

