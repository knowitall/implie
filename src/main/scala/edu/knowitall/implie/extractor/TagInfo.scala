package edu.knowitall.implie.extractor

import edu.knowitall.tool.typer.Type

/**
 * Class storing information about a tagged word and its context.
 */
class TagInfo(tag_ : String, text_ : String, intervalStart_ : Int, intervalEnd_ : Int) {
  def tag = tag_
  def text = text_
  def intervalStart = intervalStart_
  def intervalEnd = intervalEnd_
  def index = intervalEnd_

  def this(typ: Type) = this(typ.name, typ.text, typ.tokenInterval.start, typ.tokenInterval.end)

  def asIndexedString = new IndexedString(text, index)

  override def toString: String = {
    s"$tag: $text-$index"
  }

  override def hashCode(): Int = tag.hashCode() + text.hashCode() + index.hashCode()

  override def equals(other: Any): Boolean = other match {
    case o: TagInfo =>
      o.tag.equals(this.tag) &&
      o.text.equals(this.text) &&
      o.index.equals(this.index)
    case _ => false
  }
}
