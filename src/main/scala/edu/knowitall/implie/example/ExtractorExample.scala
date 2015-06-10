package edu.knowitall.implie.example

import edu.knowitall.implie.{TaggerLoader, HighRecallImplIE}

/**
 * Example use of the extractor.
 * TaggerLoader takes care of creating the out-of-the-box taggers.
 * Custom user keyword lists should be added by following the procedure in
 * TaggerLoader to create taggers compatible with ImplIE.
 */
object ExtractorExample {
  val sentences = Array(
    "French journalist Paul Legall reported that all three hostages arrived safely at Athens International Airport.",
    "John Arterberry, executive deputy chief of the fraud section in the Justice Department, said federal prosecutors and the FBI had made progress on mortgage fraud."
  )
  def main(args: Array[String]) {
    // NOTE: the highRecallTagger uses a large set of keywords and may take up
    // to a minute to load.
    val tagger = TaggerLoader.fastTagger
    //val recallTagger = TaggerLoader.highRecallTagger
    // val extractor = new ImplicitRelationExtractor(tagger)
    // val extractor = new ImplicitRelationExtractorNoLists(tagger)
    // val extractor = new ImplIEWithBasicFilters(tagger)
    // val extractor = new FormalConstrainedImplIE(tagger)
    // val extractor = new HighRecallImplIE(recallTagger)
    val extractor = new HighRecallImplIE(tagger)

    for (sentence <- sentences) {
      val extractions = extractor.extractRelations(sentence)
      println(s"Sentence: $sentence")

      for (extraction <- extractions) {
        println(s"extraction:$extraction")
      }
      println()
    }
  }
}
