package edu.knowitall.implie.util

import edu.stanford.nlp.trees.{TypedDependency, Tree}
import edu.knowitall.implie.extractor.ImplicitRelation

/**
 * Printing functions (used for debugging).
 */
object PrintUtils {

  // Prints a list of NounToNounRelations.
  private def printNounToNounRelations(relns: Iterable[ImplicitRelation]) {
    println("NounToNounRelations")
    for (reln <- relns) {
      println(reln)
    }
    println()
  }

  // Prints the labels of a tree.
  private def printLabels(tree: Tree) {
    println(s"${tree.label()}, ${tree.isLeaf}")
    for (child <- tree.children()) {
      printLabels(child)
    }
  }

  // Prints details a of tdl.
  private def printTdl(tdl: List[TypedDependency], message: String) {
    println(message)
    for (i <- 0 until tdl.size){
      val td = tdl(i)
      println(s"${td.dep()}, ${td.gov()}, ${td.reln().getShortName}, ${td.dep().tag()}")
    }
    println()
  }
}
