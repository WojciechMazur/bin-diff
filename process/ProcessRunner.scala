package process

/** Result of running an external process */
enum ProcessResult:
  case Success(stdout: String, stderr: String)
  case Failure(exitCode: Int, stdout: String, stderr: String)

  def isSuccess: Boolean = this match
    case _: Success => true
    case _: Failure => false

  def stdout: String
  def stderr: String

  def toEither: Either[String, String] = this match
    case Success(out, _) => Right(out)
    case Failure(code, _, err) => Left(s"Process failed with exit code $code: $err")

/** Abstraction for running external processes.
  * This allows for easy testing and customization of process execution.
  */
trait ProcessRunner:
  /** Run a command with the given arguments.
    * @param cmd the command to run
    * @param args arguments to pass to the command
    * @return ProcessResult containing stdout, stderr, and exit status
    */
  def run(cmd: String, args: String*): ProcessResult

  /** Run a command with environment variables.
    * @param cmd the command to run
    * @param env environment variables to set
    * @param args arguments to pass to the command
    * @return ProcessResult containing stdout, stderr, and exit status
    */
  def runWithEnv(cmd: String, env: Map[String, String], args: String*): ProcessResult

  /** Run a command in a specific working directory.
    * @param cmd the command to run
    * @param cwd working directory
    * @param args arguments to pass to the command
    * @return ProcessResult containing stdout, stderr, and exit status
    */
  def runInDir(cmd: String, cwd: os.Path, args: String*): ProcessResult

/** Default implementation using os-lib */
class DefaultProcessRunner extends ProcessRunner:
  override def run(cmd: String, args: String*): ProcessResult =
    runImpl(os.proc(cmd, args), os.pwd, Map.empty)

  override def runWithEnv(cmd: String, env: Map[String, String], args: String*): ProcessResult =
    runImpl(os.proc(cmd, args), os.pwd, env)

  override def runInDir(cmd: String, cwd: os.Path, args: String*): ProcessResult =
    runImpl(os.proc(cmd, args), cwd, Map.empty)

  private def runImpl(
      proc: os.proc,
      cwd: os.Path,
      env: Map[String, String]
  ): ProcessResult =
    try
      val result = proc.call(
        cwd = cwd,
        env = env,
        check = false, // Don't throw on non-zero exit
        mergeErrIntoOut = false
      )
      if result.exitCode == 0 then
        ProcessResult.Success(result.out.text(), result.err.text())
      else
        ProcessResult.Failure(result.exitCode, result.out.text(), result.err.text())
    catch
      case e: os.SubprocessException =>
        ProcessResult.Failure(-1, "", s"Process execution failed: ${e.getMessage}")
      case e: Exception =>
        ProcessResult.Failure(-1, "", s"Unexpected error: ${e.getMessage}")

/** Logging wrapper that logs all process invocations */
class LoggingProcessRunner(
    delegate: ProcessRunner,
    logCommand: (String, Seq[String]) => Unit = (cmd, args) =>
      println(s"[CMD] $cmd ${args.mkString(" ")}"),
    logResult: ProcessResult => Unit = result =>
      result match
        case ProcessResult.Success(_, _) => println("[OK]")
        case ProcessResult.Failure(code, _, err) =>
          println(s"[FAIL] exit=$code: ${err.take(100)}")
) extends ProcessRunner:

  override def run(cmd: String, args: String*): ProcessResult =
    logCommand(cmd, args)
    val result = delegate.run(cmd, args*)
    logResult(result)
    result

  override def runWithEnv(cmd: String, env: Map[String, String], args: String*): ProcessResult =
    logCommand(cmd, args)
    val result = delegate.runWithEnv(cmd, env, args*)
    logResult(result)
    result

  override def runInDir(cmd: String, cwd: os.Path, args: String*): ProcessResult =
    logCommand(cmd, args)
    val result = delegate.runInDir(cmd, cwd, args*)
    logResult(result)
    result

/** Process runner that caches results (useful for repeated calls) */
class CachingProcessRunner(delegate: ProcessRunner) extends ProcessRunner:
  private val cache = scala.collection.mutable.Map.empty[(String, Seq[String]), ProcessResult]

  override def run(cmd: String, args: String*): ProcessResult =
    cache.getOrElseUpdate((cmd, args.toSeq), delegate.run(cmd, args*))

  override def runWithEnv(cmd: String, env: Map[String, String], args: String*): ProcessResult =
    // Don't cache env-specific runs as env may change
    delegate.runWithEnv(cmd, env, args*)

  override def runInDir(cmd: String, cwd: os.Path, args: String*): ProcessResult =
    cache.getOrElseUpdate((cmd, cwd.toString +: args.toSeq), delegate.runInDir(cmd, cwd, args*))

