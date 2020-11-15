package slang.lex

import scala.collection.mutable
import scala.collection.Searching.Found
import scala.collection.Searching.InsertionPoint
import slang.lex.Loc

/**
  * A source file, containing line metadata.
  *
  * @param name the name of the file
  * @param source the contents of the file
  */
class SourceFile(val name: String, val source: String) {

  /**
    * Array of the line starting positions in the file.
    */
  val lineIndices = processLineIndices(source)

  private def processLineIndices(source: String) = {
    val indices = mutable.ArrayBuffer[Int](0)

    for ((c, i) <- source.zipWithIndex) {
      if (c == '\n' && i + 1 < source.length) {
        indices.addOne(i + 1)
      }
    }

    indices.toArray
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
