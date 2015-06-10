package edu.knowitall.implie.extractor

import edu.stanford.nlp.trees.TypedDependency

/**
 * Class representing an implicit relation.  Returned by ImplicitRelationExtractor
 * and subclasses.
 *
 * tag = tagged phrase
 * relation = class (nationality, jobTitle, etc)
 * np = noun phrase
 * sentence = original sentence
 * relationTrace = trace of dependency relations hopped to make the extraction
 */
class ImplicitRelation(t: TagInfo, r: String, n: IndexedSubstring,
                         s: String = "", rt: List[TypedDependency] = Nil) {
  var tag = t
  var relation = r
  var np = n
  var sentence = s
  var relationTrace = rt
  var explicitRelationTraces: List[List[TypedDependency]] = Nil
  var ners: List[NERTag] = Nil
  var head: IndexedString = null

  override def toString: String = s"(${tag.asIndexedString}, $relation, $np)"
  def longString: String = s"($tag, $relation, $np), $sentence"

  def addNER(nerTag: NERTag) {
    ners = nerTag::ners
  }

  def setNERs(nerTags: List[NERTag]) {
    ners = nerTags
  }

  def getNERs = ners

  def setHead(_head: IndexedString) {
    head = _head
  }

  def setExplicitRelationTraces(ert: List[List[TypedDependency]]) {
    explicitRelationTraces = ert
  }

  def getExplicitRelationTraces = explicitRelationTraces

  override def hashCode(): Int = tag.hashCode() + relation.hashCode +
    np.hashCode() + sentence.hashCode

  override def equals(other: Any): Boolean = other match {
    case o: ImplicitRelation =>
        o.tag.equals(this.tag) &&
        o.relation.equals(this.relation) &&
        o.np.equals(this.np) &&
        o.sentence.equals(this.sentence)
    case _ => false
  }
}
