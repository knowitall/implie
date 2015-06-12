package edu.knowitall.implie.example

import edu.knowitall.implie.{HighPrecisionImplIE, HighRecallImplIE, TaggerLoader}
import edu.knowitall.implie.extractor.{ImplicitRelation, ImplIE}
import edu.knowitall.implie.util.ParseEntry
import edu.knowitall.tool.chunk.ChunkedToken
import edu.stanford.nlp.ling.{Sentence, Word}
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.trees.PennTreebankLanguagePack

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util

import scala.collection.JavaConversions._

/**
 * ImplIE supports serialization of the parses and chunking(tokenization)
 * of the sentences to speed up extraction for sentences that are processed
 * frequently.
 *
 * Makes the biggest difference with long sentences, which take a long time to
 * parse.  With many short sentences, loading the serialized parse may take
 * longer than just parsing it on the fly.
 */
object SerializationExample {

  val serializedTokenFile = "cache/tokenization/serialization_example.ser"
  val serializedParseFile = "cache/parses/serialization_example.ser"

  val sentences = Array(
    "Staging a surprise detour from his trip to Sydney, Bush arrived at the Al-Asad air base in Anbar along with Secretary of State Condoleezza Rice and national security adviser Stephen Hadley.",
    "John Arterberry, executive deputy chief of the fraud section in the Justice Department, said federal prosecutors and the FBI had made progress on mortgage fraud.",
    "Because in reality what Spurlock shows in \"Osama\" is that the majority of regular Muslim folk in the many countries he visits -- Israel, Pakistan, Afghanistan, Egypt, Jordan -- have no use for Osama and his tactics."
  )

  def main(args: Array[String]) {
    val tagger = TaggerLoader.highPrecisionTagger
    val parser = LexicalizedParser.loadModel("models/englishPCFG.ser.gz")


    // All of the tokenization and parses from the files are loaded when the
    // extractor is constructed.  Any new ones during the lifetime of the
    // tagger is added to the serialization files.
    val cachedExtractor = new HighPrecisionImplIE(tagger, serializedTokenFile, serializedParseFile)
    val notCachedExtractor = new HighPrecisionImplIE(tagger)

    val cachedExtractions = collection.mutable.ListBuffer[ImplicitRelation]()
    val notCachedExtractions = collection.mutable.ListBuffer[ImplicitRelation]()

    val cachedStart = System.currentTimeMillis()
    for (sentence <- sentences) {
      val extractions = cachedExtractor.extractRelations(sentence)
      cachedExtractions.appendAll(extractions)
    }
    val cachedEnd = System.currentTimeMillis()

    val notCachedStart = System.currentTimeMillis()
    for (sentence <- sentences) {
      val extractions = notCachedExtractor.extractRelations(sentence)
      notCachedExtractions.appendAll(extractions)
    }
    val notCachedEnd = System.currentTimeMillis()

    println("Time (ms)")
    println(s"Cached: ${cachedEnd - cachedStart}")
    println(s"Not cached: ${notCachedEnd - notCachedStart}")
    assert(cachedEnd - cachedStart < notCachedEnd - notCachedStart)

    println("Extractions")
    println(cachedExtractions)
    println(notCachedExtractions)
    assert(cachedExtractions == notCachedExtractions)
  }
}
