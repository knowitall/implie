package edu.knowitall.implie.extractor

import edu.stanford.nlp.ling.IndexedWord


/**
 * TODO: add comment, especially constructor params
 * Created by Gene on 11/21/2014.
 */
class IndexedString(s: String, i: Int) {
  def string = s
  def index = i

  def this(iWord: IndexedWord) = this(iWord.value().toLowerCase, iWord.index())

  def contains(other: IndexedString) =
    string.contains(other.string) && index >= other.index

  override def toString: String = s"$string-$index"

  override def hashCode(): Int = string.hashCode() + index.hashCode()

  override def equals(other: Any): Boolean = other match {
    case o: IndexedString => o.string.equals(this.string) && o.index.equals(this.index)
    case _ => false
  }
}

object IndexedString {
  def emptyInstance = new IndexedString("", -1)
}