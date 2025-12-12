import analysis.*
import process.*

/** Tests for C++ symbol demangling */
class DemanglerTest extends munit.FunSuite:
  val runner: ProcessRunner = DefaultProcessRunner()

  override def beforeEach(context: BeforeEach): Unit =
    Demangler.clearCache()

  test("isMangledCpp identifies Linux-style mangled names"):
    assert(Demangler.isMangledCpp("_Z3foov"))
    assert(Demangler.isMangledCpp("_ZN3foo3barEv"))
    assert(Demangler.isMangledCpp("_ZSt4cout"))
    assert(Demangler.isMangledCpp("_ZNSt3__112basic_stringIcNS_11char_traitsIcEENS_9allocatorIcEEEC1Ev"))

  test("isMangledCpp identifies macOS-style mangled names"):
    assert(Demangler.isMangledCpp("__Z3foov"))
    assert(Demangler.isMangledCpp("__ZN3foo3barEv"))
    assert(Demangler.isMangledCpp("__ZNSt3__112basic_stringIcNS_11char_traitsIcEENS_9allocatorIcEEEC1Ev"))

  test("isMangledCpp rejects non-C++ symbols"):
    assert(!Demangler.isMangledCpp("main"))
    assert(!Demangler.isMangledCpp("printf"))
    assert(!Demangler.isMangledCpp("__libc_start_main"))
    assert(!Demangler.isMangledCpp("_start"))
    assert(!Demangler.isMangledCpp(""))

  test("demangle returns None for non-C++ symbols"):
    assertEquals(Demangler.demangle("main", runner), None)
    assertEquals(Demangler.demangle("printf", runner), None)
    assertEquals(Demangler.demangle("__start", runner), None)

  test("demangle correctly demangles simple C++ function"):
    // _Z3foov = foo()
    val result = Demangler.demangle("_Z3foov", runner)
    assert(result.isDefined, "Expected demangling to succeed")
    assertEquals(result.get, "foo()")

  test("demangle correctly demangles namespaced function"):
    // _ZN3foo3barEv = foo::bar()
    val result = Demangler.demangle("_ZN3foo3barEv", runner)
    assert(result.isDefined, "Expected demangling to succeed")
    assertEquals(result.get, "foo::bar()")

  test("demangle correctly demangles function with parameters"):
    // _Z3addii = add(int, int)
    val result = Demangler.demangle("_Z3addii", runner)
    assert(result.isDefined, "Expected demangling to succeed")
    assertEquals(result.get, "add(int, int)")

  test("demangle correctly demangles macOS-style symbol"):
    // __Z3foov = foo() (macOS adds extra underscore)
    val result = Demangler.demangle("__Z3foov", runner)
    assert(result.isDefined, "Expected demangling to succeed")
    assertEquals(result.get, "foo()")

  test("demangle caches results"):
    val symbol = "_Z3foov"
    val result1 = Demangler.demangle(symbol, runner)
    val result2 = Demangler.demangle(symbol, runner)
    assertEquals(result1, result2)

  test("demangleBatch handles multiple symbols"):
    val symbols = Seq("_Z3foov", "_Z3addii", "main", "_ZN3foo3barEv")
    val result = Demangler.demangleBatch(symbols, runner)

    assert(result.contains("_Z3foov"), "Expected foo() to be demangled")
    assert(result.contains("_Z3addii"), "Expected add(int, int) to be demangled")
    assert(result.contains("_ZN3foo3barEv"), "Expected foo::bar() to be demangled")
    assert(!result.contains("main"), "main should not be in demangle map")

  test("demangleBatch returns empty map for no mangled symbols"):
    val symbols = Seq("main", "printf", "_start")
    val result = Demangler.demangleBatch(symbols, runner)
    assert(result.isEmpty, "Expected empty map for non-C++ symbols")

  test("formatSymbol shows demangled name in parentheses"):
    val result = Demangler.formatSymbol("_Z3foov", runner)
    assert(result.contains("_Z3foov"), "Should contain original symbol")
    assert(result.contains("foo()"), "Should contain demangled name")
    assert(result.contains("(") && result.contains(")"), "Should have parentheses")

  test("formatSymbol returns original for non-C++ symbol"):
    val result = Demangler.formatSymbol("main", runner)
    assertEquals(result, "main")

  test("formatSymbolCached uses provided map"):
    val demangleMap = Map("_Z3foov" -> "foo()")
    assertEquals(Demangler.formatSymbolCached("_Z3foov", demangleMap), "_Z3foov (foo())")
    assertEquals(Demangler.formatSymbolCached("main", demangleMap), "main")

