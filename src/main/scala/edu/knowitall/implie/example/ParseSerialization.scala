package edu.knowitall.implie.example

import edu.knowitall.implie.TaggerLoader
import edu.knowitall.implie.extractor.ImplIEBase
import edu.knowitall.implie.util.ParseEntry
import edu.knowitall.tool.chunk.ChunkedToken
import edu.stanford.nlp.ling.{Sentence, Word}
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.trees.PennTreebankLanguagePack

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util

import scala.collection.JavaConversions._

/**
 * TODO: write serialization example
 * Created by Gene on 1/11/2015.
 */
object ParseSerialization {
  def main(args: Array[String]) {
    val tagger = TaggerLoader.highPrecisionTagger
    val parser = LexicalizedParser.loadModel("models/englishPCFG.ser.gz")

    val sentence = "Staging a surprise detour from his trip to Sydney, Bush arrived at the Al-Asad air base in Anbar along with Secretary of State Condoleezza Rice and national security adviser Stephen Hadley."
    val sentence2 = "The Czech government of Prime Minister Topolanek has backed Georgia in the recent conflict over South Ossetia."

    val chunks = tagger.chunker.chunk(sentence)
    val chunks2 = tagger.chunker.chunk(sentence2)
    val serials = chunks.map(ChunkedToken.stringFormat.write)
    val serializedSentence = (sentence /: serials) (_ + "\t" + _)
    println(serializedSentence)

    val sers = serializedSentence.split("\t").tail
    val chunkedTokens = sers.map(ChunkedToken.stringFormat.read)
    println(chunkedTokens.toList)

    val parse = parser.apply(Sentence.toCoreLabelList(chunks.toList.map(a => new Word(a.string))))
    val parse2 = parser.apply(Sentence.toCoreLabelList(chunks2.toList.map(a => new Word(a.string))))
    val tlp = new PennTreebankLanguagePack
    val list = tlp.grammaticalStructureFactory
               .newGrammaticalStructure(parse)
               .typedDependenciesCCprocessed
    val list2 = tlp.grammaticalStructureFactory
                .newGrammaticalStructure(parse2)
                .typedDependenciesCCprocessed
    val entry = new ParseEntry(sentence, parse, new util.ArrayList(list))
    val entry2 = new ParseEntry(sentence2, parse2, new util.ArrayList(list2))

    val fileOut = new FileOutputStream("cache/testing.ser")
    val out = new ObjectOutputStream(fileOut)
    out.writeObject(entry)
    out.writeObject(entry2)
    out.close()
    fileOut.close()

    val fileIn = new FileInputStream("cache/testing.ser")
    val in = new ObjectInputStream(fileIn)

    val newParse = in.readObject().asInstanceOf[ParseEntry]
//    val newParse2 = in.readObject().asInstanceOf[ParseEntry]
    fileIn.close()
    in.close()

    println(newParse.sentence)
    println(newParse.tree.pennString())
    println(newParse.tdl)

/*
    println(newParse2.sentence)
    println(newParse2.tree.pennString())
    println(newParse2.list)
*/

    val fileOut2 = new FileOutputStream("cache/testing.ser", true)
    val out2 = new ObjectOutputStream(fileOut2)
    out2.writeObject(entry2)
//    out2.writeObject(entry)
    out2.close()
    fileOut2.close()

    val fileIn2 = new FileInputStream("cache/testing.ser")
    val in2 = new ObjectInputStream(fileIn2)

    val newParse00 = in2.readObject().asInstanceOf[ParseEntry]
    val newParse01 = in2.readObject().asInstanceOf[ParseEntry]
/*
    val newParse02 = in2.readObject().asInstanceOf[ParseEntry]
    val newParse03 = in2.readObject().asInstanceOf[ParseEntry]
*/
    fileIn2.close()
    in2.close()

    println(newParse00.sentence)
    println(newParse00.tree.pennString())
    println(newParse00.tdl)

    println(newParse01.sentence)
    println(newParse01.tree.pennString())
    println(newParse01.tdl)

/*
    println(newParse02.sentence)
    println(newParse02.tree.pennString())
    println(newParse02.list)

    println(newParse03.sentence)
    println(newParse03.tree.pennString())
    println(newParse03.list)
*/
  }

  def timing(start: Long, end: Long, action: String) {
    val millis = (end - start) / 1000000
    println(s"$action took $millis milliseconds")
  }

  def testTiming(sentence: String) = {
    val tagger = TaggerLoader.highPrecisionTagger
    val extractor = new ImplIEBase(tagger)
    val parser = LexicalizedParser.loadModel("models/englishPCFG.ser.gz")

    val start = System.nanoTime()

    val tokens = extractor.getTokens(sentence)

    val tokenizing = System.nanoTime()
    timing(start, tokenizing, "Tokenizing")


    val tokenizedSentence = tokens.toList.map(a => new Word(a.string))

    val wordify = System.nanoTime()
    timing(tokenizing, wordify, "Wordifying")

    val rawWords = Sentence.toCoreLabelList(tokenizedSentence)

    val corelabeling = System.nanoTime()
    timing(wordify, corelabeling, "Corelabeling")

    val parse = parser.apply(rawWords)

    val parsing = System.nanoTime()
    timing(tokenizing, parsing, "Parsing")

    val tlp = new PennTreebankLanguagePack
    val list = tlp.grammaticalStructureFactory
               .newGrammaticalStructure(parse)
               .typedDependenciesCCprocessed.toList

    val listing = System.nanoTime()
    timing(parsing, listing, "Listifying")
  }
}
