package edu.knowitall.implie.extractor

/**
 * Created by Gene on 1/2/2015.
 */
class IndexedSubstring(s: String, bWordIndex: Int, eWordIndex: Int, bOffset: Int, eOffset: Int)
    extends IndexedString(s, eWordIndex) {
  private var sourceSentence: String = null

  def this(s: String, bWordIndex: Int, eWordIndex: Int, bOffset: Int, eOffset: Int, src: String) = {
    this(s, bWordIndex, eWordIndex, bOffset, eOffset)
    sourceSentence = src
  }

  // Word offsets.  Both inclusive.
  def beginWordIndex = bWordIndex
  def endWordIndex = eWordIndex
  // Substring offsets.  Begin is inclusive, end is exclusive.
  def beginOffset = bOffset
  def endOffset = eOffset
  def source = sourceSentence

  def setSource(newSource: String) {
    sourceSentence = newSource
  }

  override def hashCode(): Int = string.hashCode() + index.hashCode() +
    beginWordIndex.hashCode() + beginOffset.hashCode() + endOffset.hashCode()

  override def equals(other: Any): Boolean = other match {
    case o: IndexedSubstring =>
        o.string.equals(this.string) &&
        o.index.equals(this.index) &&
        o.beginWordIndex.equals(this.beginWordIndex) &&
        o.beginOffset.equals(this.beginOffset) &&
        o.endOffset.equals(this.endOffset)
    case _ => false
  }
}

object IndexedSubstring {
  def emptyInstance = new IndexedSubstring("", -1, -1, -1, -1)
}
