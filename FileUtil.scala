package com.lightbend.training.util
import java.io._
import scala.io.Source

/**
 * Miscellaneous file utilities.
 * They only work for the local file system, not HDFS.
 * Some are very thin wrappers around the corresponding `java.io.File` methods.
 */
object FileUtil {

  implicit class StringToFile(s: String) {
    def toFile: File = new File(s)
  }

  /**
   * Recursively, forcibly delete one or more paths.
   * @return true if successful or the paths don't exist, or return false if any delete attempt fails.
   */
  def rmrf(path: File, paths: File*): Boolean = rmrf(path +: paths)

  /**
   * Recursively, forcibly delete one or more paths.
   * @return true if successful or the paths don't exist, or return false if any delete attempt fails.
   */
  def rmrf(paths: Seq[File]): Boolean =
    doAllPredicate(paths) {
      case path if path.isFile => path.delete()
      case path if path.exists =>
        doAllPredicate(path.listFiles)(f => rmrf(f)) && path.delete()
      case _ => true  // doesn't exist
    }

  /**
   * Delete one or more paths. If any is a non-empty directory,
   * it won't work.
   * @return true if successful or the path doesn't exist, or return false on failure.
   */
  def rm(path: File, paths: File*): Boolean = rm(path +: paths)

  /**
   * Delete one or more paths. If any is a non-empty directory,
   * it won't work.
   * @return true if successful or the path doesn't exist, or return false on failure.
   */
  def rm(paths: Seq[File]): Boolean =
    doAllPredicate(paths)(path => path.delete)

  /**
   * Make one or more directories, including parents as needed
   * (like the `-p` option for the *NIX `mkdir` command).
   * @return true if successful or false if not.
   */
  def mkdirs(path: File, paths: File*): Boolean = mkdirs(path +: paths)

  /**
   * Make one or more directories, including parents as needed
   * (like the `-p` option for the *NIX `mkdir` command).
   * @return true if successful or false if not.
   */
  def mkdirs(paths: Seq[File]): Boolean =
    doAllPredicate(paths)(path => path.mkdirs)

  /**
   * List ("ls") one or more files and/or directories.
   * @return Set[File] with contents or empty if input path doesn't exist.
   * @note Does not return a "." entry for the directory itself, if a path is a directory.
   */
  def ls(path: File, paths: File*): Seq[File] = ls(path +: paths)

  /**
   * List ("ls") one or more files and/or directories.
   * @return Set[File] with contents or empty if input path doesn't exist.
   * @note Does not return a "." entry for the directory itself, if a path is a directory.
   */
  def ls(paths: Seq[File]): Seq[File] =
    paths.flatMap { path =>
      if (path.exists == false) Vector.empty[File]
      else if (path.isFile) Vector(path)
      else path.listFiles.toVector
    }

  /**
   * List ("ls") one or more files and/or directories, recursively.
   * @return Vector[File] with contents or empty if input path doesn't exist.
   */
  def lsr(path: File, paths: File*): Vector[File] = lsr(path +: paths)

  /**
   * List ("ls") one or more files and/or directories, recursively.
   * Also has an optional flag to suppress directory entries, themselves.
   * @return Vector[File] with contents or empty if input path doesn't exist.
   */
  def lsr(paths: Seq[File], suppressDirectories: Boolean = false): Vector[File] = {
    def _lsr(accum: Vector[File], paths2: Vector[File]): Vector[File] = {
      paths2.foldLeft(accum) { (accum, p) =>
        if (p.exists == false) accum
        else if (p.isFile) accum :+ p
        else {
          val accum2 = if (suppressDirectories) accum else accum :+ p
          _lsr(accum2, p.listFiles.toVector)
        }
      }
    }
    _lsr(Vector.empty[File], paths.toVector)
  }

  var indentDelta     = "| "
  var directoryPrefix = "+ "
  var filePrefix      = ""
  var formatString    = "%s%s%-25s (%d bytes)"

  /**
   * Pretty print a file or the contents of a directory, recursively.
   */
  def print(path: File): Unit = {
    def p(index: Int, file: File): Unit = {
      if (file.exists != false) {
        val indent = indentDelta * index
        val fd = if (file.isDirectory) directoryPrefix else filePrefix
        println(formatString.format(
          indent, fd, file.getName, file.length))
        if (file.isDirectory) {
          file.listFiles.foreach(f2 => p(index+1, f2))
        }
      }
    }
    p(0, path)
  }

  private def doAllPredicate[T](paths: Seq[T])(f: T => Boolean): Boolean =
    paths.foldLeft(true)((success, path) => f(path) && success)
}