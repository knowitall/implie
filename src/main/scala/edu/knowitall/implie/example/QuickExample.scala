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
    val extractor = ImplIELoader.defaultImplIE
    // Version that loads faster.
//    val extractor = ImplIELoader.fastImplIE
    // Other versions.
    // ImplIELoader.highPrecisionImplIE // -- same as fastImplIE
    // ImplIELoader.highRecallImplIE    // -- same as defaultImplIE

    for (sentence <- sentences) {
      val extractions = extractor.extractRelations(sentence)
      println(s"Sentence: $sentence")

      for (extraction <- extractions) {
        println(s"extraction: $extraction")
        // Extraction information broken down.
        println(s"tag term: ${extraction.tag.text}, tag class:" +
          s" ${extraction.relation}, extraction(NP): ${extraction.np}")
        // Get the head word for just the entity.
        // NOTE: the head is only a single token.  Will not include full names.
        println(s"head word: ${extraction.head}")
      }
      println()
    }
  }
}
