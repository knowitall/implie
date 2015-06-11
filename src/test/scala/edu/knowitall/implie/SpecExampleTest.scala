package edu.knowitall.implie

/**
 * Basic test to make sure ImplIE is working correctly.
 * Runs the sentence that is used for the example in the repo README.
 */
object SpecExampleTest {
  val sentence = "French journalist Paul Legall reported that all three hostages arrived safely at Athens International Airport."
  val expectedString = "(Athens-13, city, Athens International Airport-15)\n(French-1, nationality, French journalist Paul Legall-4)\n(journalist-2, jobTitle, journalist Paul Legall-4)"

  def main(args: Array[String]) {
    val extractor = ImplIELoader.fastImplIE
    val extractions = extractor.extractRelations(sentence)
    val extractionStrs = extractions.map(e => e.toString).sorted
    println(extractionStrs.mkString("\n"))

    // There should be three extractions.
    assert(extractions.size == 3)
    // Check that the extractions are the expected extractions.
    assert(extractionStrs.mkString("\n") == expectedString)

    println("PASS: all tests passed!")
  }
}
