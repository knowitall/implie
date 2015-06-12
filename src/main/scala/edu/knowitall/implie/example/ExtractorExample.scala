package edu.knowitall.implie.example

import com.typesafe.config.ConfigFactory
import edu.knowitall.implie.{TaggerLoader, HighRecallImplIE}

/**
 * Example use of the extractor.
 *
 * We construct a new custom extractor for extracting from excerpts of Tolkien's
 * books about Middle Earth by adding a class for Middle Earth races.
 *
 * Custom user keyword lists should be added by following procedure using
 * TaggerLoader to create taggers compatible with ImplIE.
 *
 * The custom tags for this example are in class_term_lists/custom/example/
 */
object ExtractorExample {
  // A few sentences from Lord of the Rings: Fellowship of the Ring.
  val lotrSentences = Array(
    "But Nob, the hobbit servant, came bustling in long before they thought of ringing.",
    "‘And I heard she pushed him in, and he pulled her in after him,’ said Sandyman, the Hobbiton miller.",
    "Maybe he would have attacked Bilbo at once, if the ring had been on him when they met; but it was not, and the hobbit held in his hand an Elvish knife, which served him as a sword."
  )

  val newsSentences = Array(
    "French journalist Paul Legall reported that all three hostages arrived safely at Athens International Airport.",
    "John Arterberry, executive deputy chief of the fraud section in the Justice Department, said federal prosecutors and the FBI had made progress on mortgage fraud."
  )

  def main(args: Array[String]) {
    // ConfigFactory loads files relative to src/main/resources
    // Take a look at the custom tagger configuration.  We removed city,
    // province, and religion, and added middleEarthRace.
    val taggerConfig = ConfigFactory.load("taggers/custom/example-custom-tagger.conf")
    val customTagger = TaggerLoader.buildTagger(taggerConfig)
    val extractor = new HighRecallImplIE(customTagger)

    println()
    println("Extracting from sentences in LotR: Fellowship of the Ring.")
    for (sentence <- lotrSentences) {
      val extractions = extractor.extractRelations(sentence)
      println(s"Sentence: $sentence")

      for (extraction <- extractions) {
        println(s"extraction:$extraction")
      }
      println()
    }

    println()
    println("Extracting from news sentences.")
    println("Notice that we no longer extract non-middle earth cities, countries, and provinces.")
    println("We could imagine also removing some of the job titles that won't be in" +
      "Middle Earth, but they're unlikely to cause problems if we leave them in.")
    for (sentence <- newsSentences) {
      val extractions = extractor.extractRelations(sentence)
      println(s"Sentence: $sentence")

      for (extraction <- extractions) {
        println(s"extraction:$extraction")
      }
      println()
    }
  }
}
