package edu.knowitall.implie.extractor

import edu.stanford.nlp.ling.CoreLabel

/**
 * Simple class (pretty much a struct) for keeping track of an NER tag.
 * Meant for simplicity and to be able to express multiword NER tags so we
 * can merge tags together if adjacent.
 */
class NERTag(
  entityStr: String,
  nerAnswer: String,
  bIndex: Int,
  eIndex: Int,
  indexedTokens: List[(CoreLabel, Int)]) {

  def entityString = entityStr
  def ner = nerAnswer
  def beginIndex = bIndex
  def endIndex = eIndex
  def tokens = indexedTokens

  override def toString =
    s"$entityStr\t$nerAnswer\t$bIndex\t$eIndex\t$indexedTokens"
}
