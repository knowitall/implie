package edu.knowitall.implie

/**
 * Basic test to make sure ImplIE is working correctly.
 * Runs the sentence that is used for the example in the repo README.
 */
object SpecExampleTest {
  val sentence = "French journalist Paul Legall reported that all three hostages arrived safely at Athens International Airport."
  def main(args: Array[String]) {
    val extractor = ImplIELoader.fastImplIE
    val extractions = extractor.extractRelations(sentence)
    val extractionStrs = extractions.map(e => e.toString)
    println(extractionStrs.mkString("\n"))
    // TODO: asseert extraction number
    // TODO: check each extraction
  }
}
