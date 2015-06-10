package edu.knowitall.implie.example

import edu.knowitall.implie.ImplIELoader

/**
 * The most basic usage example of ImplIE.
 */
object QuickExample {
  val sentences = Array(
    "French journalist Paul Legall reported that all three hostages arrived safely at Athens International Airport.",
    "John Arterberry, executive deputy chief of the fraud section in the Justice Department, said federal prosecutors and the FBI had made progress on mortgage fraud."
  )

  def main(args: Array[String]) {
    // The default ImplIE system takes a while to load since it uses a large
    // keyword set.  Use the commented version below if you don't care which
    // version is used.
    //
    // NOTE: For the example sentences above, the resulting
    // are the same.  In general, however, the default system generates more
    // extractions but with more errors than the fast loading version.
// TODO: uncomment top one.
//    val extractor = ImplIELoader.defaultImplIE
    // Versions that loads faster.
    val extractor = ImplIELoader.fastImplIE

    for (sentence <- sentences) {
      val extractions = extractor.extractRelations(sentence)
      println(s"Sentence: $sentence")

      for (extraction <- extractions) {
        println(s"extraction: $extraction")
        println(extraction.head)
        println(extraction.tag)
        println(extraction.relation)
        println(extraction.tag)
      }
      println()
    }
  }
}
