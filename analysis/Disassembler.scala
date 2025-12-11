package analysis

import model.*
import process.*

/** Disassembles binaries using objdump */
class Disassembler(runner: ProcessRunner):

  /** Disassemble a binary and return a map of function name -> instructions */
  def disassemble(path: os.Path): Either[String, Map[String, DisassembledFunction]] =
    // Use objdump -d for disassembly
    // -d: disassemble executable sections
    // On macOS, we might need to use llvm-objdump or otool
    val result = runner.run("objdump", "-d", path.toString)

    result match
      case ProcessResult.Success(stdout, _) =>
        Right(parseObjdumpOutput(stdout))
      case ProcessResult.Failure(code, _, stderr) =>
        // Try llvm-objdump as fallback (common on macOS)
        val llvmResult = runner.run("llvm-objdump", "-d", path.toString)
        llvmResult match
          case ProcessResult.Success(stdout, _) =>
            Right(parseObjdumpOutput(stdout))
          case ProcessResult.Failure(_, _, llvmErr) =>
            // Try otool as last resort (macOS native)
            val otoolResult = runner.run("otool", "-tV", path.toString)
            otoolResult match
              case ProcessResult.Success(stdout, _) =>
                Right(parseOtoolOutput(stdout))
              case ProcessResult.Failure(_, _, otoolErr) =>
                Left(s"Disassembly failed. objdump: $stderr, llvm-objdump: $llvmErr, otool: $otoolErr")

  /** Parse GNU objdump / LLVM objdump output */
  private def parseObjdumpOutput(output: String): Map[String, DisassembledFunction] =
    val functions = scala.collection.mutable.Map.empty[String, DisassembledFunction]
    var currentFunction: Option[String] = None
    var currentAddress: Long = 0L
    var currentInstructions = List.newBuilder[Instruction]

    // objdump function header pattern: "0000000000001234 <function_name>:"
    val functionHeaderRegex = """^([0-9a-fA-F]+)\s+<([^>]+)>:""".r
    // Instruction line pattern: "    1234:	48 89 e5             	mov    %rsp,%rbp"
    val instructionRegex = """^\s*([0-9a-fA-F]+):\s+([0-9a-fA-F ]+?)\s+(\S+)\s*(.*)$""".r
    // Alternative pattern for some objdump versions
    val altInstructionRegex = """^\s*([0-9a-fA-F]+):\s+(\S+)\s*(.*)$""".r

    def saveCurrentFunction(): Unit =
      currentFunction.foreach { name =>
        val instructions = currentInstructions.result()
        if instructions.nonEmpty then
          functions(name) = DisassembledFunction(name, currentAddress, instructions)
      }

    for line <- output.linesIterator do
      line match
        case functionHeaderRegex(addr, name) =>
          saveCurrentFunction()
          currentFunction = Some(name)
          currentAddress = parseHexAddress(addr).getOrElse(0L)
          currentInstructions = List.newBuilder[Instruction]

        case instructionRegex(addr, bytes, mnemonic, operands) if currentFunction.isDefined =>
          val instruction = Instruction(
            address = parseHexAddress(addr).getOrElse(0L),
            bytes = bytes.trim,
            mnemonic = mnemonic,
            operands = operands.trim,
            rawLine = line
          )
          currentInstructions += instruction

        case altInstructionRegex(addr, mnemonic, operands) if currentFunction.isDefined =>
          val instruction = Instruction(
            address = parseHexAddress(addr).getOrElse(0L),
            bytes = "",
            mnemonic = mnemonic,
            operands = operands.trim,
            rawLine = line
          )
          currentInstructions += instruction

        case _ => // Skip other lines

    saveCurrentFunction()
    functions.toMap

  /** Parse macOS otool -tV output */
  private def parseOtoolOutput(output: String): Map[String, DisassembledFunction] =
    val functions = scala.collection.mutable.Map.empty[String, DisassembledFunction]
    var currentFunction: Option[String] = None
    var currentAddress: Long = 0L
    var currentInstructions = List.newBuilder[Instruction]

    // otool function header pattern: "_main:" or "(__TEXT,__text) section" followed by function names
    val functionHeaderRegex = """^([_a-zA-Z][_a-zA-Z0-9]*):$""".r
    // Instruction pattern: "0000000100003f50	pushq	%rbp"
    val instructionRegex = """^([0-9a-fA-F]+)\s+(\S+)\s*(.*)$""".r

    def saveCurrentFunction(): Unit =
      currentFunction.foreach { name =>
        val instructions = currentInstructions.result()
        if instructions.nonEmpty then
          functions(name) = DisassembledFunction(name, currentAddress, instructions)
      }

    for line <- output.linesIterator do
      line match
        case functionHeaderRegex(name) =>
          saveCurrentFunction()
          currentFunction = Some(name)
          currentInstructions = List.newBuilder[Instruction]

        case instructionRegex(addr, mnemonic, operands) if currentFunction.isDefined =>
          val parsedAddr = parseHexAddress(addr).getOrElse(0L)
          if currentInstructions.knownSize == 0 then
            currentAddress = parsedAddr
          val instruction = Instruction(
            address = parsedAddr,
            bytes = "",
            mnemonic = mnemonic,
            operands = operands.trim,
            rawLine = line
          )
          currentInstructions += instruction

        case _ => // Skip other lines

    saveCurrentFunction()
    functions.toMap

  private def parseHexAddress(hex: String): Option[Long] =
    try Some(java.lang.Long.parseUnsignedLong(hex, 16))
    catch case _: NumberFormatException => None

object Disassembler:
  def apply(runner: ProcessRunner): Disassembler = new Disassembler(runner)

