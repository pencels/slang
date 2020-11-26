package slang.sourcemap

import scala.collection.mutable.ArrayBuffer
import scala.collection.Searching.Found
import scala.collection.Searching.InsertionPoint
import scala.io.Source
import java.io.FileNotFoundException

/**
  * A source file, containing line metadata.
  *
  * @param name the name of the file
  * @param source the contents of the file
  */
class SourceFile(val name: String, val source: String, val startPos: Int) {

  /**
    * Array of the line starting positions in the file.
    */
  val lineIndices = processLineIndices(source)

  private def processLineIndices(source: String) = {
    val indices = ArrayBuffer[Int](0)

    for ((c, i) <- source.zipWithIndex) {
      if (c == '\n' && i + 1 < source.length) {
        indices.addOne(i + 1)
      }
    }

    indices.toArray
  }

  /**
    * Gets the source line associated with a position.
    *
    * @param pos a start-offset position
    */
  def lineString(pos: Int) = {
    val line = lineLookup(pos).get
    val start = lineIndices.lift(line).getOrElse(0)
    val end = lineIndices.lift(line + 1).getOrElse(source.length)
    source.slice(start, end)
  }

  /**
    * The slice of the source at the given line.
    *
    * @param line the 1-indexed line number
    */
  def getSourceAtLine(line: Int) = {
    val start = lineIndices.lift(line - 1).getOrElse(0)
    val end = lineIndices.lift(line).getOrElse(source.length)
    source.slice(start, end)
  }

  /**
    * The line and column of a given character position.
    *
    * @param index the character position
    * @return the Loc of the character position
    */
  def locationOfCharPos(pos: Int) = {
    val line = lineLookup(pos).get
    val col = pos - lineIndices(line)

    Loc(line + 1, col + 1)
  }

  /**
    * The 0-indexed line for the given character position. If the file
    * is empty (no lines of code) or the position would come before
    * the first line, returns `None`.
    *
    * @param pos the character position to lookup
    */
  private def lineLookup(pos: Int): Option[Int] = {
    if (lineIndices.isEmpty) {
      return None
    }
    lineIndices.search(pos) match {
      case Found(foundIndex)              => Some(foundIndex)
      case InsertionPoint(0)              => None
      case InsertionPoint(insertionPoint) => Some(insertionPoint - 1)
    }
  }

  /**
    * Snaps the position to the beginning of the line it sits on.
    *
    * @param pos the position to snap
    */
  def lineBeginPos(pos: Int) = lineIndices(lineLookup(pos).get)
}

class SourceMap {
  private var _lastPos = 0
  private val filePositions = ArrayBuffer[Int]()
  private val files = ArrayBuffer[SourceFile]()

  def lastPos = _lastPos

  /**
    * Add "virtual" (e.g. stdin) file to the source map. Does not load from the path.
    *
    * @param path the path of the file
    * @param source the source of the file
    */
  def addVirtualFile(path: String, source: String) = {
    filePositions.addOne(_lastPos)
    files.addOne(new SourceFile(path, source, _lastPos))
    _lastPos += source.length
  }

  /**
    * Add file to the source map.
    *
    * @param path the path of the file to append.
    */
  def addFile(path: String) = {
    val source = Source.fromFile(path).toString()
    filePositions.addOne(_lastPos)
    files.addOne(new SourceFile(path, source, _lastPos))
    _lastPos += source.length
  }

  /**
    * Gets the substring of source code held in this source map.
    *
    * @param start the inclusive starting index.
    * @param end the exclusive ending index.
    * @return a substring of characters in the source code between the `[start, end)` indices.
    * Does NOT cross file boundaries - the starting index determines which file from which
    * the source will be read.
    */
  def slice(start: Int, end: Int): String = {
    // Find the file containing the start index.
    fileLookup(start) match {
      case Some(file) =>
        file.source.slice(start - file.startPos, end - file.startPos)
      case None =>
        throw new Exception(
          s"Getting slice [$start, $end) that does not correspond to a file."
        )
    }
  }

  /**
    * The source line containing the given char position.
    *
    * @param pos the character position in the source map
    */
  def lineString(pos: Int): String = {
    fileLookup(pos) match {
      case Some(file) => file.lineString(pos - file.startPos)
      case None =>
        throw new Exception(
          s"Getting position $pos that does not correspond to a file."
        )
    }
  }

  /**
    * The file and location of the char pos in a given file.
    *
    * @param pos the character position
    */
  def location(pos: Int): (SourceFile, Loc) = {
    fileLookup(pos) match {
      case Some(file) => (file, file.locationOfCharPos(pos - file.startPos))
      case None =>
        throw new Exception(
          s"Getting position $pos that does not correspond to a file."
        )
    }
  }

  /**
    * Gets the `SourceFile` for the given character position. If the `SourceMap`
    * contains no files or the position is out of bounds, will return None.
    *
    * @param pos the character position to look up
    */
  def fileLookup(pos: Int): Option[SourceFile] = {
    if (filePositions.isEmpty) {
      return None
    }
    val index = filePositions.search(pos) match {
      case Found(foundIndex)              => foundIndex
      case InsertionPoint(0)              => return None
      case InsertionPoint(insertionPoint) => insertionPoint - 1
    }

    Some(files(index))
  }
}
