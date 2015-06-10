package edu.knowitall.implie.extractor

import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.sentence
import edu.knowitall.taggers.TaggerCollection

/**
 * ImplIE with all basic filters.
 *
 * List of filters:
 *  NN of ...
 *  ... vs. ...
 *  Days of week at beginning or end or entity
 *  strange date formats (... UTC)
 */
class HighPrecisionImplIE(
  tagger: TaggerCollection[sentence.Sentence with Chunked with Lemmatized],
  serializedTokenCacheFile: String = null,
  serializedParseCacheFile: String = null)
  extends ImplIENoLists(
    tagger, serializedTokenCacheFile, serializedParseCacheFile)
  with BasicFilterFunctions {

  override def extractRelations(line: String): List[ImplicitRelation] = {
    val relations = super.extractRelations(line)

    var filtered = relations
    filtered = filterNNOfEntities(line, filtered)
    filtered = filterDaysOfWeek(line, filtered)
    filtered = filterStrangeDateFormats(line, filtered)
    filtered = filterVsEntities(line, filtered)
    filtered = filterTagIsEntity(line, filtered)
    filtered
  }

}
