package edu.knowitall.implie.util

import java.io.Serializable

import edu.stanford.nlp.trees.{Tree, TypedDependency}

/**
 * A single parse.  Organization of parse used for serialization.
 */
class ParseEntry(_sentence: String, _tree: Tree, _tdl: java.util.ArrayList[TypedDependency]) extends Serializable {
  def sentence = _sentence
  def tree = _tree
  def tdl = _tdl
}