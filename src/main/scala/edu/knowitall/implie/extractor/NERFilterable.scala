package edu.knowitall.implie.extractor

import com.typesafe.config.Config
import edu.knowitall.tool.chunk.ChunkedToken
import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.ling.{CoreAnnotations, CoreLabel, Word}
import edu.knowitall.implie.util.ExtractionUtils

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * Trait for any class that wants to be able to filter extractions by NER tags.
 *
 * Most notably, it provides the filterNERs function.
 */
trait NERFilterable {
  protected val NER_MODEL: String
  protected val classifier: CRFClassifier[CoreLabel]

  def getTokens(line: String): Seq[ChunkedToken]
  def tagNERs(extractions: List[ImplicitRelation],
              line: String, nerTagsToIgnore: List[String]): List[ImplicitRelation] = {
    // Run NER tagger and pair in the entity portion (check the indicies).
    val wordTokens = getTokens(line).map(token => new Word(token.string))
    val rawNERTags = classifier.classifySentence(wordTokens)

    // Filter the default NER tag and group NER tags together by answer annotations.
    val nerTags =
      rawNERTags
      // Add token indicies to ner tags.
      .foldLeft(Nil: List[(CoreLabel, Int)], 0)((acc, cur) =>
        ((cur, acc._2)::acc._1, acc._2 + 1))._1.reverse
      .filter(tag => !nerTagsToIgnore.contains(tag._1.get(classOf[CoreAnnotations.AnswerAnnotation])))
      .foldLeft(Nil: List[NERTag])((acc, cur) => {
        val curNER = cur._1.get(classOf[CoreAnnotations.AnswerAnnotation])
        val curIndex = cur._2
        acc match {
          case Nil => new NERTag(cur._1.word, curNER, curIndex, curIndex, cur::Nil)::Nil
          case head::tail =>
            if (curNER == head.ner && curIndex == head.endIndex + 1) {
              new NERTag(
                ExtractionUtils.substringFromWordIndicies(line, getTokens(line), head.beginIndex, curIndex),
                curNER, head.beginIndex, curIndex, cur::head.tokens)::tail
            } else {
              new NERTag(cur._1.word, curNER, curIndex, curIndex, cur::Nil)::
                head::tail
            }
        }
      }).map(tag => new NERTag(tag.entityString, tag.ner, tag.beginIndex,
        tag.endIndex, tag.tokens.reverse))

    // For each extraction find the NER tags that are within the extraction bounds.
    extractions.foldLeft(Nil: List[ImplicitRelation])(
      (acc, cur) => {
        val (beginIndex, endIndex) = (cur.np.beginWordIndex, cur.np.endWordIndex)
        val nersWithinExtraction =
          nerTags.foldLeft(Nil: List[NERTag])((acc, cur) => {
            val tokensWithin = cur.tokens.filter(
              pair => pair._2 >= beginIndex && pair._2 <= endIndex)
            if (tokensWithin.size == 0) {
              acc
            } else {
              val (curBegin, curEnd) = (tokensWithin(0)._2,
                tokensWithin(tokensWithin.size - 1)._2)
              new NERTag(
                ExtractionUtils.substringFromWordIndicies(line, getTokens(line), curBegin, curEnd),
                cur.ner, curBegin, curEnd, tokensWithin) :: acc
            }
          })
        cur.setNERs(nersWithinExtraction)
        cur::acc
      })
  }

  def expectedTagEntities(confs: List[Config]): Map[String, List[String]] = {
    val map = mutable.Map[String, List[String]]()
    for (conf <- confs) {
      val tag = conf.getString("tag")
      val entityTypes = conf.getStringList("entity-types").toList
      map.put(tag, entityTypes)
    }
    map.toMap
  }
}
