package edu.knowitall.implie.util

import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.typer.Type
import edu.stanford.nlp.trees.Tree
import edu.knowitall.implie.extractor.{IndexedString, TagInfo}

import scala.collection.JavaConversions._

/**
 * General utility functions for extractions.
 */
object ExtractionUtils {
  // Pre: each index can have at most 1 tag
  // Creates a mapping from the token index to the TagInfo.
  def createTagMap(tags: List[Type]): Map[Int, TagInfo] = {
    def tagMapHelper (acc: Map[Int, TagInfo], tags: List[Type]): Map[Int, TagInfo] = {
      tags match {
        case Nil => acc
        case (tag :: tail) =>
          val newacc = acc + ((tag.tokenInterval.end, new TagInfo(tag)))
          tagMapHelper(newacc, tail)
      }
    }
    tagMapHelper(Map(), tags)
  }

// Extracts a substring from token/word indices rather than character indices.
  def substringFromWordIndicies(
    string: String,
    tokens: Seq[ChunkedToken],
    beginIndex: Int,
    endIndex: Int): String = {

    string.substring(tokens(beginIndex).offset,
      tokens(endIndex).offset + tokens(endIndex).string.length)
  }


  /**
   * Returns a list of indexed strings that represent a flattened version
   * of the tree.  Should be readable as a sentence.
   */
  def phraseTokensFromTree(tree: Tree): List[IndexedString] = {
    val labels = tree.`yield`().map(l => l.toString).toList
    // Only labels of leaf nodes will have a dash.
    // All others will be a name for a phrase type.
    labels.filter(l => l.contains("-")).map(l => {
      val (string, negIndex) = l.splitAt(l.lastIndexOf('-'))
      // Indices are interpreted as negative because of the dash, so flip.
      new IndexedString(string, 0 - negIndex.toInt)
    })
  }
}
