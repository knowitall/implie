package edu.knowitall.implie.util

import edu.knowitall.tool.typer.Type
import edu.stanford.nlp.trees.TypedDependency
import edu.knowitall.implie.extractor.{ImplIE, TagInfo}

import scala.collection.mutable

/**
 * Utility functions for obtaining different output formats for extractions
 * including verbose output and the extraction trace.
 */
object ExtractionFormatUtils {
  def verboseOutput(extractor: ImplIE)(src: String) = {
    // Parse tree + dependency list
    val parse = extractor.getParse(src)
    val builder = new mutable.StringBuilder
    builder.append(parse._1.pennString())
    for (td <- parse._2) {
      builder.append(td + "\n")
    }
    builder.mkString
  }

  def extractionInfo(extractor: ImplIE)(src: String): String = {
    def printHops(map: Map[TagInfo, List[List[TypedDependency]]], builder: StringBuilder) {
      for ((k, v) <- map) {
        builder.append(s"Tag: ${k.text} has tag ${k.tag}\tDependency Hops:\t")
        for (td <- v) {
          builder.append(s"$td\t")
        }
        builder.append("\n")
      }
    }
    val tags = extractor.getTags(src)
    val (parse, tdl) = extractor.getParse(src)
    val singleHops = tagToDependencyMap(tags, tdl)
    val singleGeneralized = singleHops map { case (k, v) => (k, v.map(td => td::Nil))}
    val doubleHops = expandAllByOneHop(singleGeneralized, tdl)

    val builder = mutable.StringBuilder.newBuilder
    builder.append("Extraction Info\n")

    for ((k, v) <- singleHops) {
      builder.append(s"Tag: ${k.text} has tag ${k.tag}\tDependency Hops:\t")
      builder.append("Single Hops ")
      for (td <- v) {
        builder.append(s"$td ")
      }
      builder.append("\t")
      builder.append("Double Hops ")
      for (td <- doubleHops.getOrElse(k, Nil)) {
        builder.append(s"$td ")
      }
      builder.append("\n")
    }

    builder.mkString
  }


  /*

  Helper methods.

   */

  /*
    To get hops from tag:
      Get word that is modified by tag: the govenor word
      go through every dependency, where the tagged word is included.
      Get all dependencies where the dep of the previous
    */
  // Pre: tdl has all the tagged dependencies for a specific tag
  // tagged and tdl must come from the same extraction.  Otherwise the result is undefined.
  // Pull out any hop that includes the tag's govenor word.
  def getSingleHops(tagged: List[TypedDependency], tdl: List[TypedDependency]): List[TypedDependency] = {
    tagged.map(tagDep => tdl.filter(td => td.gov().endPosition() == tagDep.gov().endPosition() || td.dep().endPosition() == tagDep.gov().endPosition())).flatten
  }

  def expandByOneHop(curtdl: List[List[TypedDependency]], tdl: List[TypedDependency]): List[List[TypedDependency]] = {
    curtdl.foldLeft(List[List[TypedDependency]]())((acc, cur: List[TypedDependency]) =>
      tdl.filter(td =>
        (cur.head.dep().index() == td.dep().index() ||
          cur.head.dep().index() == td.gov().index() ||
          cur.head.gov().index() == td.dep().index() ||
          cur.head.gov().index() == td.gov().index()) &&
          !cur.contains(td)
      ).map(td => td::cur):::acc
    )
  }

  def expandAllByOneHop(tdlMaps: Map[TagInfo, List[List[TypedDependency]]],
    tdl: List[TypedDependency]): Map[TagInfo, List[List[TypedDependency]]] = {
    val newMap = mutable.Map[TagInfo, List[List[TypedDependency]]]()
    for ((k, v) <- tdlMaps) {
      newMap.put(k, expandByOneHop(v, tdl))
    }
    newMap.toMap
  }

  // Create a map of each tag to a list of TypedDependency objects that contain
  // that tag.
  def tagToDependencyMap(tags: List[Type],
    tdl: List[TypedDependency]): Map[TagInfo, List[TypedDependency]] = {

    val tagMap = ExtractionUtils.createTagMap(tags)
    val results = mutable.Map[TagInfo, List[TypedDependency]]()
    for (td <- tdl) {
      tagMap.get(td.dep().index()) match {
        case None => null
        case Some(tagInfo: TagInfo) =>
          results.get(tagInfo) match {
            case None => results.put(tagInfo, td::Nil)
            case Some(current) => results.put(tagInfo, td::current)
          }
      }
      tagMap.get(td.gov().index()) match {
        case None => null
        case Some(tagInfo: TagInfo) =>
          results.get(tagInfo) match {
            case None => results.put(tagInfo, td::Nil)
            case Some(current) => results.put(tagInfo, td::current)
          }
      }
    }
    results.toMap
  }
}
