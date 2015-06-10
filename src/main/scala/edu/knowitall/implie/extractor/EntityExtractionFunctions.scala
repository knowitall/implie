package edu.knowitall.implie.extractor

import edu.knowitall.tool.chunk.ChunkedToken
import edu.stanford.nlp.ling.IndexedWord
import edu.stanford.nlp.trees.{TypedDependency, Tree}
import edu.knowitall.implie.util.ExtractionUtils

import scala.collection.JavaConversions._

/**
 * Set of entity extraction functions.
 *
 * Various functions to pull out a entity substring from extraction information.
 * Many of them attempt to find a noun phrase entity.
 *
 * Each of the functions take the following parameters
 *  Tree, represents that entire sentence that the extraction is from
 *  List[TypedDependency], list with the extraction expansions
 *  TagInfo, information about the tagged term for the implicit relation extracted
 *  Seq[ChunkedToken], tokenized version of the sentence
 *  String, the original sentence
 *  ImplicitRelationExtractor, the edu.knowitall.implie.extractor used in the extraction.
 *
 * and returns
 *  IndexedSubstring, a substring of the sentence representing the entity for
 *    the extraction.
 */
object EntityExtractionFunctions {
  /**
   * Takes the leftmost and rightmost nodes in the Tree from the extraction
   * dependency list, then creates a substring entity from the subtree rooted
   * by the first common ancestor that is a noun phrase.
   */
  def firstNounPhraseAncestor(
    tree: Tree,
    tdl: List[TypedDependency],
    tag: TagInfo,
    tokenizedSentence: Seq[ChunkedToken],
    sentence: String,
    extractor: ImplIEBase): IndexedSubstring = {

    if (tdl.size == 0) {
      return null
    }
    // get the leftmost and rightmost terms.
    val indexedWordTag = tree.`yield`().toList
                         .slice(tag.intervalStart, tag.intervalEnd)
                         .map(x => new IndexedWord(x))
    val (leftmost, rightmost) = getLeftAndRight(indexedWordTag(0),
      indexedWordTag(indexedWordTag.size - 1), tdl)

    // Get root of noun phrase that contains both the dependent and govenor words.
    val npRoot = getNPAncestor(tree, leftmost, rightmost)._1

    // Cut out the appropriate noun phrase from the sentence.
    // Phrase tokens don't have character offsets so get them from the chunked sentence.
    val phraseTokens = ExtractionUtils.phraseTokensFromTree(npRoot)
    val firstChunk = tokenizedSentence(phraseTokens(0).index - 1)
    val lastChunk = tokenizedSentence(
      phraseTokens(phraseTokens.size - 1).index - 1)

    val (startIndex, endIndex, wordIndex) = extendToEnclosePunctuation(
      tree, sentence, firstChunk.offset,
      lastChunk.offset + lastChunk.string.length,
      phraseTokens(0).index - 1, phraseTokens(phraseTokens.size - 1).index,
      extractor)

    new IndexedSubstring(
      sentence.substring(startIndex, endIndex),
      phraseTokens(0).index - 1,
      wordIndex,
      startIndex,
      endIndex,
      sentence
    )
  }

  /**
   * Identifies a parent noun phrase of the tag and the first TypedDependency.
   * Then expands on the noun phrase using the tdl to create the complete 
   * entity substring.
   */
  def expandFromSmallNP(
    tree: Tree,
    tdl: List[TypedDependency],
    tag: TagInfo,
    tokenizedSentence: Seq[ChunkedToken],
    sentence: String,
    extractor: ImplIEBase): IndexedSubstring = {

    if (tdl.size == 0) {
      return null
    }
    // get the leftmost and rightmost terms.
    val indexedWordTag = tree.`yield`().toList
                         .slice(tag.intervalStart, tag.intervalEnd)
                         .map(x => new IndexedWord(x))
    // Start with NP that only includes the tag and the first extraction step.
    val firstTd = tdl match {
      case head::tail => head::Nil
      case Nil => Nil
    }
    val (smallLeftmost, smallRightmost) = getLeftAndRight(indexedWordTag(0),
      indexedWordTag(indexedWordTag.size - 1), firstTd)


    val npRoot = getNPAncestor(tree, smallLeftmost, smallRightmost)._1

    // left and right without np root.
    val (left, right) = getLeftAndRight(indexedWordTag(0),
      indexedWordTag(indexedWordTag.size - 1), tdl)

    // Cut out the appropriate noun phrase from the sentence.
    // Phrase tokens don't have character offsets so get them from the chunked sentence.
    val phraseTokens = ExtractionUtils.phraseTokensFromTree(npRoot)
    val firstChunkIndex = Math.min(phraseTokens(0).index, left.index) - 1
    val lastChunkIndex = Math.max(phraseTokens(phraseTokens.size - 1).index, right.index()) - 1
    val firstChunk = tokenizedSentence(firstChunkIndex)
    val lastChunk = tokenizedSentence(lastChunkIndex)

    val (startIndex, endIndex, wordIndex) = extendToEnclosePunctuation(
      tree, sentence, firstChunk.offset,
      lastChunk.offset + lastChunk.string.length,
      firstChunkIndex, lastChunkIndex + 1,
      extractor)

    new IndexedSubstring(
      sentence.substring(startIndex, endIndex),
      firstChunkIndex,
      wordIndex,
      startIndex,
      endIndex,
      sentence
    )
  }

  /**
   * Extracts the smallest substring of the sentence that contains all
   * components of the tdl.
   */
  def smallestSubstring(
    tree: Tree,
    tdl: List[TypedDependency],
    tag: TagInfo,
    tokenizedSentence: Seq[ChunkedToken],
    sentence: String,
    extractor: ImplIEBase): IndexedSubstring = {

    if (tdl.size == 0) {
      return null
    }
    // get the leftmost and rightmost terms.
    val indexedWordTag = tree.`yield`().toList
                         .slice(tag.intervalStart, tag.intervalEnd)
                         .map(x => new IndexedWord(x))

    // left and right without np root.
    val (left, right) = getLeftAndRight(indexedWordTag(0),
      indexedWordTag(indexedWordTag.size - 1), tdl)

    val firstChunkIndex = left.index - 1
    val lastChunkIndex = right.index() - 1
    val firstChunk = tokenizedSentence(firstChunkIndex)
    val lastChunk = tokenizedSentence(lastChunkIndex)

    val (startIndex, endIndex, wordIndex) = extendToEnclosePunctuation(
      tree, sentence, firstChunk.offset,
      lastChunk.offset + lastChunk.string.length,
      firstChunkIndex, lastChunkIndex + 1,
      extractor)

    new IndexedSubstring(
      sentence.substring(startIndex, endIndex),
      firstChunkIndex,
      wordIndex,
      startIndex,
      endIndex,
      sentence
    )
  }

  /**
   * Extracts the smallest substring of the sentence that contains all
   * components of the tdl after each noun component is expanded to it's parent
   * if and only if the parent is an NP.
   */
  def smallestSubstringWithParentNPs
    (tree: Tree,
     tdl: List[TypedDependency],
     tag: TagInfo,
     tokenizedSentence: Seq[ChunkedToken],
     sentence: String,
     extractor: ImplIEBase): IndexedSubstring = {

    /*
    Expand tag to the parent if the parent is an NP.
    Expand any NN or NNP leaves to the parent if it's an NP.
    Then find the smallest substring.
     */
    if (tdl.size == 0) {
      return null
    }

    // get the leftmost and rightmost terms.
    val indexedWordTag = tree.`yield`().toList
      .slice(tag.intervalStart, tag.intervalEnd)
      .map(x => new IndexedWord(x))
    // Start with NP that only includes the tag and the first extraction step.
    val firstTd = tdl match {
      case head::tail => head::Nil
      case Nil => Nil
    }
    val (tagleft, tagright) = getLeftAndRight(indexedWordTag(0),
      indexedWordTag(indexedWordTag.size - 1), firstTd)

    // left and right from just the tdl.
    val (tdlleft, tdlright) = getLeftAndRight(indexedWordTag(0),
      indexedWordTag(indexedWordTag.size - 1), tdl)


    val leaves = tree.getLeaves[Tree]()
    val relns = Set("NP", "NNP", "np", "nnp")
    val terms = tdl.foldLeft(List(tagleft, tagright): List[IndexedWord])((acc, cur) => cur.gov()::cur.dep()::acc)

    // Get parents of the leaves if the parent is an NP, else just return the leaf.
    val nproots = terms.map(iw => {
      leaves.foldLeft(null: Tree)((acc, cur) => {
        if (acc != null) {
          acc
        } else {
          if (cur.label().toString.equalsIgnoreCase(s"${iw.value()}-${iw.index()}")) {
            // Found it.
            val node = cur.parent(tree) // This isn't acutally the parent, it's the encapsulating tree node for the leaf.
            if (relns.contains(node.parent(tree).label().toString)) {
              node.parent(tree)
            } else {
              node
            }
          } else {
            null
          }
        }
      })
    })

    // Calculate min and max indices for all np chunks in the expansion.
    val (npmin, npmax) = nproots
      .map(nproot => nproot.getLeaves[Tree]().toList)
      .foldLeft(Nil: List[Tree])((acc, cur) => cur:::acc)
      .foldLeft((-1, -1): (Int, Int))((acc, cur) => {
        val dashSplits = cur.label().toString.split("-")
        val index = dashSplits(dashSplits.size - 1).toInt
        if (acc._1 == -1) {
          (index, index)
        } else {
          if (index < acc._1) {
            (index, acc._2)
          } else if (index > acc._2) {
            (acc._1, index)
          } else {
            acc
          }
        }
      })

    // Calculate min and max indices for tag, nps, and expansion rules.
    val firstChunkIndex = Math.min(tagleft.index(), Math.min(tdlleft.index(), npmin)) - 1
    val lastChunkIndex = Math.max(tagright.index(), Math.max(tdlright.index(), npmax)) - 1
    val firstChunk = tokenizedSentence(firstChunkIndex)
    val lastChunk = tokenizedSentence(lastChunkIndex)

    val (startIndex, endIndex, wordIndex) = extendToEnclosePunctuation(
      tree, sentence, firstChunk.offset,
      lastChunk.offset + lastChunk.string.length,
      firstChunkIndex, lastChunkIndex + 1,
      extractor)

    new IndexedSubstring(
      sentence.substring(startIndex, endIndex),
      firstChunkIndex,
      wordIndex,
      startIndex,
      endIndex,
      sentence
    )
  }

  /*

  Helper methods

   */

  /**
   * Returns the subtree of the first common ancestor of the two indexed words
   * that is a noun phrase.
   */
  def getNPAncestor(tree: Tree, dep: IndexedWord,
    gov: IndexedWord): (Tree, Boolean, Boolean) = {
    if (tree.isLeaf) {
      val label = tree.label().toString
      if (label.equalsIgnoreCase(s"${dep.value()}-${dep.index()}")) {
        (tree, true, false)
      } else if (label.equalsIgnoreCase(s"${gov.value()}-${gov.index()}")) {
        (tree, false, true)
      } else {
        (tree, false, false)
      }
    } else {
      val childResults = tree.children()
                         .map(t => getNPAncestor(t, dep, gov))
      val correctChildren = childResults.filter(
        t => t._1.label().toString == "NP" && t._2 && t._3)
      if (correctChildren.size > 0) {
        return correctChildren(0)
      }
      val curResult = childResults.foldLeft((false, false))(
        (acc, cur) => (acc._1 || cur._2, acc._2 || cur._3))
      (tree, curResult._1, curResult._2)
    }
  }

    /**
     * Gets the indexed words that represent the left most and rightmost indices.
     */
  def getLeftAndRight(
    startLeft: IndexedWord,
    startRight: IndexedWord,
    tdl: List[TypedDependency]): (IndexedWord, IndexedWord) = {

    tdl.foldLeft(startLeft, startRight)((acc, cur) =>
      acc match {
        case (null, null) =>
          if (cur.dep.index() < cur.gov.index()) {
            (cur.dep, cur.gov)
          } else {
            (cur.gov, cur.dep)
          }
        case _ =>
          val (accleft, accright, curd, curg) = (acc._1, acc._2, cur.dep, cur.gov)
          List(accleft, accright, curd, curg)
          .foldLeft((accleft, accright))((acc, cur) => {
            val left = if (acc._1.index() < cur.index()) {
              acc._1
            } else {
              cur
            }
            val right = if (acc._2.index() > cur.index()) {
              acc._2
            } else {
              cur
            }
            (left, right)
          })
      })
  }

  /**
   * Determines the new offsets after extending the string extraction window
   * to close any open paired punctuation.  The specified punctuation to do this
   * is specified in high-precision-edu.knowitall.implie.extractor.conf in the field "enclosing-punctuation".
   *
   * @param tree Tree parse of the entire sentence.
   * @param sentence Entire source sentence string.
   * @param start Current starting character index of the extraction window. (inclusive)
   * @param end Current ending character index of the extraction window. (exclusive)
   * @param firstWordIndex Current starting parse index of the extraction window.
   * @param lastWordIndex Current ending parse index of the extraction window.
   * @param extractor ImplicitRelationExtractor which contains enclosing
   *                  punctuation and cached sentence tokenization info.
   * @return A triple of ints.  Th first and second are the character offsets
   *         of the sentence for extracting the phrase after extending to
   *         close all open punctuation.  The third is the parse index, which
   *         needs to be added to NounToNounRelations.
   */
  def extendToEnclosePunctuation(tree: Tree, sentence: String, start: Int, end: Int,
    firstWordIndex: Int, lastWordIndex: Int,
    extractor: ImplIEBase): (Int, Int, Int) = {
    val tokens = ExtractionUtils.phraseTokensFromTree(tree)
    val chunks = extractor.getTokens(sentence) // token chunks
    extractor.getEnclosingPunctuation.foldLeft(start, end, lastWordIndex)((punctAcc, punctCur) => {
      val lastOpen = tokens.lastIndexWhere(t => t.string.contains(punctCur.open), lastWordIndex)
      val lastClose = tokens.lastIndexWhere(t => t.string.contains(punctCur.close), lastWordIndex)
      lastOpen > lastClose && lastOpen > firstWordIndex match {
        case true =>
          val newLastWordIndex = tokens.indexWhere(t => t.string.contains(punctCur.close), lastWordIndex + 1)
          if (newLastWordIndex <= punctAcc._3) {
            return punctAcc
          }
          val newLastCharIndex = chunks(newLastWordIndex).offset +
            chunks(newLastWordIndex).string.length
          (punctAcc._1, newLastCharIndex, newLastWordIndex + 1)
        case false => punctAcc
      }
    })
  }
}
