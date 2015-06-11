package edu.knowitall.implie

import java.io.{PrintWriter, File}
import java.text.SimpleDateFormat
import java.util.Date

import edu.knowitall.implie.extractor.ImplIE

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Integrity Test for ImplIE.
 * For purposes of checking that the ImplIE system is working as intended
 * after any changes.
 *
 * NOTE: because of results caching, the high-recall extractor and
 * high-precision extractor will seem to run much faster than the default and
 * fast extractor.  However, one their own their runtime will be about the same.
 */
object IntegrityTest {

  case class OutTuple(np: String, relation: String, tag: String)
  case class AKitem(tup: OutTuple, correct: String, incorrect: String, sentence: String)
  case class CompItem(tup: OutTuple, sentence: String)
  case class Results(correct: Set[CompItem], incorrect: Set[CompItem], neither: Set[CompItem])
  case class Performance(precision: Double, extractions: Int)
  case class SystemData(name: String, implie: ImplIE, results: Results, expected: Results)

  val resourceDir = "src/main/resources/test/"
  val testResultDir = "test_files/test_results/"
  val sdf = new SimpleDateFormat("MM-dd-yyyy_HH:mm")
  val expectedFile = "test_files/version_results/v1.0.0-SNAPSHOT_results"

  val outputSeparator = "----DONE----"

  val headers = List("Neither Correct nor Incorrect", "Correct Results", "Incorrect Results")
  val stateChange = Map[String, String]("default" -> "neither", "neither" -> "correct", "correct" -> "incorrect")

  var precisionPass = true
  var extractionNumPass = true
  var discrepanciesPass = true

  // Expected performance info.
  def loadExpectedResults(filename: String) = {
    val file = Source.fromFile(filename).mkString
    val resultList: List[String] = file.split(outputSeparator).toList
      .filter(str => str.trim() != "")

    val expectedResults = mutable.Map[String, Results]()
    for (result <- resultList) {
      val lines = result.split("\n").toList
        .map(l => l.trim.replaceAll("\r", "")).filter(l => l != "")
      val name = lines(0)

      var state = "default"
      val binnedItems = Map[String, mutable.ListBuffer[CompItem]](
        "neither" -> new ListBuffer(),
        "correct" -> new ListBuffer(),
        "incorrect" -> new ListBuffer()
      )

      for (line <- lines) {
        val tokens = line.split("\t")
        if (tokens.size == 4) {
          // Is a result add to the results.
          val tup = OutTuple(tokens(0), tokens(1), tokens(2))
          binnedItems(state).append(CompItem(tup, tokens(3)))
        } else if(headers.contains(line.trim())) {
          // Transition point.
          state = stateChange(state)
        }
      }
      expectedResults.put(name,
        Results(binnedItems("correct").toSet, binnedItems("incorrect").toSet,
          binnedItems("neither").toSet))
    }
    expectedResults.toMap
  }

  def resultsToPerformance(results: Results) = {
    val c = results.correct.size.toFloat
    val i = results.incorrect.size.toFloat
    val n = results.neither.size.toFloat
    val t = c + i + n
    Performance(c / t, math.round(t))
  }

  def cleanSentence(sentence: String) = {
    sentence.replaceAll("\"", "")
  }

  def printlnBoth(out: PrintWriter, str: String) {
    println(str)
    out.println(str)
  }

  def main(args: Array[String]) {
    // Load sentences and answerkey
    val sentences: List[String] = Source.fromFile(resourceDir + "sentences").getLines().toList
    val answerkey: List[String] = Source.fromFile(resourceDir + "answerkey").getLines().toList

    // Generate answerkey structures
    val answerkeyTokens = answerkey.map(l => {
      val toks = l.toLowerCase.split("\t")
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

    // Create output file.
    val resultFilename = "test_result_" + sdf.format(new Date(System.currentTimeMillis())).replaceAll(":", "-")
    val out = new PrintWriter(new File(testResultDir + resultFilename))

    // Load extractors.
    println("Loading extractors for test...")
    val defaultExtractor = ImplIELoader.defaultImplIE
    val fastExtractor = ImplIELoader.fastImplIE
    val recallExtractor = ImplIELoader.highRecallImplIE
    val precisionExtractor = ImplIELoader.highPrecisionImplIE

    val expectedResults = loadExpectedResults(expectedFile)
    val expectedPerformance =
      expectedResults map {case (k, v) => (k, resultsToPerformance(v))}

    val systemMap = mutable.Map[String, SystemData](
      "default" -> SystemData("default", defaultExtractor, null, expectedResults("default")),
      "fast" -> SystemData("fast", fastExtractor, null, expectedResults("fast")),
      "recall" -> SystemData("recall", recallExtractor, null, expectedResults("recall")),
      "precision" -> SystemData("precision", precisionExtractor, null, expectedResults("precision"))
    )

    def printResults(results: Results) {
      println("Results will be printed in file, but not here for brevity.")

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
      out.println()
    }

    def printSummary(results: Results) = {
      val perf = resultsToPerformance(results)
      printlnBoth(out, "Summary")
      printlnBoth(out, s"precision ${perf.precision}")
      printlnBoth(out, s"total extractions ${perf.extractions}")
      printlnBoth(out, "")
      perf
    }

    // Run extractors on sentences.
    def runExtractor(extractor: ImplIE) = {
      val extractions = sentences.map(s => extractor.extractRelations(s)).toList.flatten
      // Turn the extractions into comparable items.
      val compExtractions = extractions.map(e => {
        val tup = OutTuple(e.np.toString.toLowerCase, e.relation.toLowerCase,
          e.tag.asIndexedString.toString.toLowerCase)
        CompItem(tup, cleanSentence(e.sentence.toLowerCase))
      }).toSet

      val correct = compExtractions & correctset
      val incorrect = compExtractions & incorrectset
      val neither = compExtractions -- (correct | incorrect)
      Results(correct, incorrect, neither)
    }


    // Run and score each extractor.
    println(s"Writing detailed results to ${testResultDir + resultFilename}")
    for ((name, sysdata) <- systemMap) {
      // Just check number of extractions and precision first.
      println()
      println("---------------------------------------------------")
      println(s"Running $name extractor")
      val results = runExtractor(sysdata.implie)
      systemMap.put(name, SystemData(sysdata.name, sysdata.implie, results, sysdata.expected))

      // Print results to file and get summary.
      printlnBoth(out, "Extractor results")
      printResults(results)
      printlnBoth(out, "")
      val perf = printSummary(results)
      out.println(outputSeparator)

      // Check if precision and number of extractions is at least what we
      // expect.
      val exp = expectedResults(name)
      val expPerf = resultsToPerformance(exp)
      if (expPerf.precision > perf.precision) {
        precisionPass = false
        println("!!!FAIL: current precision is worse than expected!!!")
        println(s"expected: ${expPerf.precision}\tactual: ${perf.precision}")
      }
      if (expPerf.extractions > perf.extractions) {
        extractionNumPass = false
        println("!!!FAIL: current number of extractions is worse than expected!!!")
        println(s"expected: ${expPerf.extractions}\tactual: ${perf.extractions}")
      }

      // Check differences in the actual extractions.
      // Make lowercase.
      def lowerItem(item: CompItem) = {
        CompItem(OutTuple(item.tup.np.toLowerCase, item.tup.relation.toLowerCase,
          item.tup.tag.toLowerCase), item.sentence.toLowerCase)
      }
      val lowerexp = Results(exp.correct.map(lowerItem),
        exp.incorrect.map(lowerItem), exp.neither.map(lowerItem))
      val lowerresults = Results(results.correct.map(lowerItem),
        results.incorrect.map(lowerItem), results.neither.map(lowerItem))

      val newcor = lowerresults.correct -- lowerexp.correct
      val missingcor = lowerexp.correct -- lowerresults.correct
      val newincor = lowerresults.incorrect -- lowerexp.incorrect
      val missingincor = lowerexp.incorrect -- lowerresults.incorrect

      // Print out discrepancies.
      if (newcor.size == 0 && missingcor.size == 0 && newincor.size == 0 &&
          missingincor.size == 0 && results.neither.size == 0 &&
          exp.neither.size == 0) {
        printlnBoth(out, "No discrepancies between current and expected extractions.")
      } else {
        discrepanciesPass = false
        printlnBoth(out, "Discrepancies between current and expected extractions.")

        if (newcor.size != 0) {
          printlnBoth(out, "Correct now, not in expected.")
          printlnBoth(out, newcor.map(c =>
            s"${c.tup.np}\t${c.tup.relation}\t${c.tup.tag}\t${c.sentence}")
            .mkString("\n"))
        }

        if (missingcor.size != 0) {
          printlnBoth(out, "Expected correct, but not now.")
          printlnBoth(out, missingcor.map(c =>
            s"${c.tup.np}\t${c.tup.relation}\t${c.tup.tag}\t${c.sentence}")
            .mkString("\n"))
        }

        if (newincor.size != 0) {
          printlnBoth(out, "Incorrect now, not in expected.")
          printlnBoth(out, newincor.map(c =>
            s"${c.tup.np}\t${c.tup.relation}\t${c.tup.tag}\t${c.sentence}")
            .mkString("\n"))
        }

        if (missingincor.size != 0) {
          printlnBoth(out, "Incorrect in expected, not now.")
          printlnBoth(out, missingincor.map(c =>
            s"${c.tup.np}\t${c.tup.relation}\t${c.tup.tag}\t${c.sentence}")
            .mkString("\n"))
        }

        if (results.neither.size != 0) {
          printlnBoth(out, "Not found.")
          printlnBoth(out, results.neither.map(c =>
            s"${c.tup.np}\t${c.tup.relation}\t${c.tup.tag}\t${c.sentence}")
            .mkString("\n"))
        }

        if (exp.neither.size != 0) {
          printlnBoth(out, "Not found in expected")
          printlnBoth(out, exp.neither.map(c =>
            s"${c.tup.np}\t${c.tup.relation}\t${c.tup.tag}\t${c.sentence}")
            .mkString("\n"))
        }
      }
    }
    out.close()

    println("---------------------------------------------------")
    if (precisionPass && extractionNumPass) {
      println("PASS: All test pass!")
    }
    if (!discrepanciesPass) {
      println("WARNING: There were some discrepancies between the expected and actual extractions.")
    }
    if (!precisionPass) {
      println("FAIL: One of the systems got less than expected precision.")
    }
    if (!extractionNumPass) {
      println("FAIL: One of the systems got less than expected number of extractions.")
    }
  }
}
