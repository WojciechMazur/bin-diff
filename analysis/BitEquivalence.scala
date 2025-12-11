package analysis

import model.*
import java.security.MessageDigest

/** Bit-level equivalence checking using SHA-256 hashing */
object BitEquivalence:

  /** Compute SHA-256 hash of a file */
  def hashFile(path: os.Path): String =
    val digest = MessageDigest.getInstance("SHA-256")
    val bytes = os.read.bytes(path)
    digest.update(bytes)
    digest.digest().map("%02x".format(_)).mkString

  /** Check if two binaries are bit-identical */
  def check(oldPath: os.Path, newPath: os.Path): BitEquivalenceResult =
    val oldSize = os.size(oldPath)
    val newSize = os.size(newPath)

    // Quick size check first - if sizes differ, files can't be identical
    if oldSize != newSize then
      // Still compute hashes for reporting
      val oldHash = hashFile(oldPath)
      val newHash = hashFile(newPath)
      BitEquivalenceResult(
        identical = false,
        oldHash = oldHash,
        newHash = newHash,
        oldSize = oldSize,
        newSize = newSize
      )
    else
      val oldHash = hashFile(oldPath)
      val newHash = hashFile(newPath)
      BitEquivalenceResult(
        identical = oldHash == newHash,
        oldHash = oldHash,
        newHash = newHash,
        oldSize = oldSize,
        newSize = newSize
      )

