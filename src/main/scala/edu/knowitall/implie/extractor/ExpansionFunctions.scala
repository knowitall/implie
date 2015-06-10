package edu.knowitall.implie.extractor

import edu.knowitall.tool.chunk.ChunkedToken
import edu.stanford.nlp.trees.TypedDependency
import extractor.{ExpansionFunction, Rule}

/**
 * Created by Gene on 12/31/2014.
 */

class ExpansionFunctions {

  private var id = ""
  private var idValue: IndexedString = null
  private var rules: List[Rule] = Nil
  private var tokens: Seq[ChunkedToken] = Nil
  private var tdl: List[TypedDependency] = Nil


  private val table = Map[String, ExpansionFunction](
    "default" -> simpleRule,
    "prep_of_no_dobj" ->  prepOfNoDobj, // not used.
    "conj_and_appos" -> conjAndAppos,
    "prep_of_no_punct" -> prepOfNoPunct,
    "appos_no_and" -> apposNoAnd,
    "nn_no_comma" -> nnNoComma
  )

  def prepareFunctions(id: String, idValue: IndexedString, rules: List[Rule],
                       tokens: Seq[ChunkedToken], tdl: List[TypedDependency]): Unit = {
    this.id = id
    this.idValue = idValue
    this.rules = rules
    this.tokens = tokens
    this.tdl = tdl
  }

  def getFunctionForRelation(relation: String): ExpansionFunction = {
    table.getOrElse(relation, simpleRule)
  }

  private def simpleRule(td: TypedDependency, rule: Rule): (TypedDependency, String, IndexedString) = {
    val matchesRelation = td.reln().toString.equals(rule.rel)
    val matchesDep = rule.dep.equals(id) &&
      tokens(idValue.index - 1).string.equals(td.dep.value()) &&
      idValue.index == td.dep.index()
    val matchesGov = rule.gov.equals(id) &&
      tokens(idValue.index - 1).string.equals(td.gov.value()) &&
      idValue.index == td.gov.index()
    val result = matchesRelation && (matchesDep || matchesGov) && !(matchesDep && matchesGov)

    if (result && matchesDep) {
      (td, rule.gov, new IndexedString(td.gov()))
    } else if (result && matchesGov) {
      (td, rule.dep, new IndexedString(td.dep()))
    } else {
      null
    }
  }

  private def prepOfNoDobj(td: TypedDependency, rule: Rule): (TypedDependency, String, IndexedString) = {
    val matchesRelation =
      td.reln().toString.equals("prep_of") &&
        rule.gov.equals(id) &&
        tokens(idValue.index - 1).string.equals(td.gov.value()) &&
        idValue.index == td.gov.index()

    // Find if any dobj matches.
    val matchingDobj =
      tdl.exists(t => t.reln().toString == "dobj" &&
        t.dep.value() == td.gov.value() &&
        t.dep.index == td.gov.index)

    if (matchesRelation && !matchingDobj) {
      (td, rule.dep, new IndexedString(td.dep()))
    } else {
      null
    }
  }

  private def conjAndAppos(td: TypedDependency, rule: Rule): (TypedDependency, String, IndexedString) = {
    val matchesRelation = td.reln().toString.equals("conj_and")
    val matchesDep = rule.dep.equals(id) &&
      tokens(idValue.index - 1).string.equals(td.dep.value()) &&
      idValue.index == td.dep.index()
    val matchesGov = rule.gov.equals(id) &&
      tokens(idValue.index - 1).string.equals(td.gov.value()) &&
      idValue.index == td.gov.index()
    val result = matchesRelation && (matchesDep || matchesGov) && !(matchesDep && matchesGov)

    // Find if there are any ands in between the two arguments.
    val (max, min) = (
        Math.max(td.gov.index, td.dep.index) - 1,
        Math.max(Math.min(td.gov.index, td.dep.index) - 1, 0))
    var foundAnd = false
    for (i <- min to max) {
      if (tokens(i).string.equalsIgnoreCase("and")) {
        foundAnd = true
      }
    }

    if (result && !foundAnd) {
      if (matchesDep) {
        (td, rule.gov, new IndexedString(td.gov()))
      } else if (matchesGov) {
        (td, rule.dep, new IndexedString(td.dep()))
      } else {
        null
      }
    } else {
      null
    }
  }

  private def apposNoAnd(td: TypedDependency, rule: Rule): (TypedDependency, String, IndexedString) = {
    val matchesRelation = td.reln().toString.equals("appos")
    val matchesDep = rule.dep.equals(id) &&
      tokens(idValue.index - 1).string.equals(td.dep.value()) &&
      idValue.index == td.dep.index()
    val matchesGov = rule.gov.equals(id) &&
      tokens(idValue.index - 1).string.equals(td.gov.value()) &&
      idValue.index == td.gov.index()
    val result = matchesRelation && (matchesDep || matchesGov) && !(matchesDep && matchesGov)

    // Find if there are any ands in between the two arguments.
    val (max, min) = (
      Math.max(td.gov.index, td.dep.index) - 1,
      Math.max(Math.min(td.gov.index, td.dep.index) - 1, 0))
    var foundAnd = false
    for (i <- min to max) {
      if (tokens(i).string.equalsIgnoreCase("and")) {
        foundAnd = true
      }
    }

    if (result && !foundAnd) {
      if (matchesDep) {
        (td, rule.gov, new IndexedString(td.gov()))
      } else if (matchesGov) {
        (td, rule.dep, new IndexedString(td.dep()))
      } else {
        null
      }
    } else {
      null
    }
  }

  private def prepOfNoPunct(td: TypedDependency, rule: Rule): (TypedDependency, String, IndexedString) = {
    val punctuation = Set(".", ",", ";", ":", "(", ")")
    val matchesRelation = td.reln().toString.equals("prep_of")
    val matchesDep = rule.dep.equals(id) &&
      tokens(idValue.index - 1).string.equals(td.dep.value()) &&
      idValue.index == td.dep.index()
    val matchesGov = rule.gov.equals(id) &&
      tokens(idValue.index - 1).string.equals(td.gov.value()) &&
      idValue.index == td.gov.index()
    val result = matchesRelation && (matchesDep || matchesGov) && !(matchesDep && matchesGov)

    // Find if there are any "of"s in between the two arguments where
    // the previous token is a punctuation march.
    val (max, min) = (
      Math.max(td.gov.index, td.dep.index) - 1,
      Math.max(Math.min(td.gov.index, td.dep.index) - 1, 0))
    var punctBeforeOf = false
    for (i <- min to max) {
      if (tokens(i).string.equalsIgnoreCase("of") && i != min &&
          punctuation.contains(tokens(i - 1).string)) {
        punctBeforeOf = true
      }
    }

    if (result && !punctBeforeOf) {
      if (matchesDep) {
        (td, rule.gov, new IndexedString(td.gov()))
      } else if (matchesGov) {
        (td, rule.dep, new IndexedString(td.dep()))
      } else {
        null
      }
    } else {
      null
    }
  }

  private def nnNoComma(td: TypedDependency, rule: Rule): (TypedDependency, String, IndexedString) = {
    val matchesRelation = td.reln().toString.equals("nn")
    val matchesDep = rule.dep.equals(id) &&
      tokens(idValue.index - 1).string.equals(td.dep.value()) &&
      idValue.index == td.dep.index()
    val matchesGov = rule.gov.equals(id) &&
      tokens(idValue.index - 1).string.equals(td.gov.value()) &&
      idValue.index == td.gov.index()
    val result = matchesRelation && (matchesDep || matchesGov) && !(matchesDep && matchesGov)

    // Find if there are any ands in between the two arguments.
    val (max, min) = (
      Math.max(td.gov.index, td.dep.index) - 1,
      Math.max(Math.min(td.gov.index, td.dep.index) - 1, 0))
    var foundComma = false
    for (i <- min to max) {
      if (tokens(i).string.equalsIgnoreCase(",")) {
        foundComma = true
      }
    }

    if (result && !foundComma) {
      if (matchesDep) {
        (td, rule.gov, new IndexedString(td.gov()))
      } else if (matchesGov) {
        (td, rule.dep, new IndexedString(td.dep()))
      } else {
        null
      }
    } else {
      null
    }
  }
}
