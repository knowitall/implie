package edu.knowitall.implie

import java.io.{PrintWriter, File}
import java.text.SimpleDateFormat
import java.util.Date

import edu.knowitall.implie.extractor.ImplIE

import scala.io.Source

/**
 * Integrity Test for ImplIE.
 * For purposes of checking that the ImplIE system is working as intended
 * after any changes.
 */
object IntegrityTest {

  case class OutTuple(np: String, relation: String, tag: String)
  case class AKitem(tup: OutTuple, correct: String, incorrect: String, sentence: String)
  case class CompItem(tup: OutTuple, sentence: String)
  case class Results(correct: Set[CompItem], incorrect: Set[CompItem], neither: Set[CompItem])

  val resourceDir = "src/main/resources/test/"
  val testResultDir = "test_results/"
  val sdf = new SimpleDateFormat("MM-dd-yyyy_HH:mm")

  def cleanSentence(sentence: String) = {
    sentence.replaceAll("\"", "")
  }

  def main(args: Array[String]) {
    // Load sentences and answerkey
    val sentences = Source.fromFile(resourceDir + "sentences").getLines()
    val answerkey = Source.fromFile(resourceDir + "answerkey").getLines()

    // Generate answerkey structures
    val answerkeyTokens = answerkey.map(l => {
      val toks = l.split("\t")
      val tup = OutTuple(toks(0), toks(1), toks(2))
      AKitem(tup, toks(3), toks(4), toks(5))
    })
    val correctset = answerkeyTokens
      .filter(item => item.correct == "1")
      .map(item => CompItem(item.tup, cleanSentence(item.sentence)))
      .toSet
    val incorrectset = answerkeyTokens
      .filter(item => item.incorrect == "1")
      .map(item => CompItem(item.tup, cleanSentence(item.sentence)))
      .toSet

    // Load extractors.
    println("Loading extractors for test...")
    val defaultExtractor = ImplIELoader.defaultImplIE
    val fastExtractor = ImplIELoader.fastImplIE
    val recallExtractor = ImplIELoader.highRecallImplIE
    val precisionExtractor = ImplIELoader.highRecallImplIE

    // Run extractors on sentences.
    def runExtractor(extractor: ImplIE) = {
      val extractions = sentences.map(s => extractor.extractRelations(s)).toList.flatten
      // Turn the extractions into comparable items.
      val compExtractions = extractions.map(e => {
        val tup = OutTuple(e.np.toString, e.relation, e.tag.asIndexedString.toString)
        CompItem(tup, cleanSentence(e.sentence))
      }).toSet

      val correct = compExtractions & correctset
      val incorrect = compExtractions & incorrectset
      val neither = compExtractions -- (correct | incorrect)
      Results(correct, incorrect, neither)
    }

    val defaultResults = runExtractor(defaultExtractor)
    val fastResults = runExtractor(fastExtractor)
    val recallResults = runExtractor(recallExtractor)
    val precisionResults = runExtractor(precisionExtractor)

    val resultFilename = "test_result_" + sdf.format(new Date(System.currentTimeMillis()))
    val out = new PrintWriter(new File(testResultDir + resultFilename))
    println(s"Writing detailed results to ${testResultDir + resultFilename}")

    def printResults(results: Results) {
      out.println()
      out.println("Neither Correct nor Incorrect")
      out.println(results.neither.map(c =>
        s"${c.tup.np}\t${c.tup.relation}\t${c.tup.tag}\t${c.sentence}")
        .mkString("\n"))
      out.println()

      out.println("Correct Results")
      out.println(results.correct.map(c =>
        s"${c.tup.np}\t${c.tup.relation}\t${c.tup.tag}\t${c.sentence}")
        .mkString("\n"))
      out.println()

      out.println("Incorrect Results")
      out.println(results.incorrect.map(c =>
        s"${c.tup.np}\t${c.tup.relation}\t${c.tup.tag}\t${c.sentence}")
        .mkString("\n"))
      out.println()
    }

    def printSummary(results: Results) {
      val c = results.correct.size
      val i = results.incorrect.size
      val n = results.neither.size
      val t = c + i + n
      out.println("Summary")
      out.println(s"preicision ${c / t}")
      out.println(s"total extractions $t")
      out.println()

      println("Summary")
      println(s"preicision ${c / t}")
      println(s"total extractions $t")
      println()
    }

    println("Default extractor results.")
    out.println("Default extractor results.")
    printResults(defaultResults)
    printSummary(defaultResults)

    println("Default extractor results.")
    out.println("Fast extractor results.")
    printResults(fastResults)
    printSummary(fastResults)

    println("Default extractor results.")
    out.println("High recall extractor results.")
    printResults(recallResults)
    printSummary(recallResults)

    println("Default extractor results.")
    out.println("High precision extractor results.")
    printResults(precisionResults)
    printSummary(precisionResults)

    // TODO: Get expected results and print out what extractions we now get wrong, and what we now get right in comparison.
    // TODO: Test that precision and total extractions is at least what we had before.

    out.close()
  }
}
