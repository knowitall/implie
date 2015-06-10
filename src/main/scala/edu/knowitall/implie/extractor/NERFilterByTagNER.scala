package edu.knowitall.implie.extractor

/**
 * NERFilterByTagNER filters out extractions which have a tag NER of not the
 * expected type.
 *
 * For edu.knowitall.implie.example, if the tag for a 'city' relation has an NER type of PERSON,
 * the extraction is filtered out. 
 * 
 * An edu.knowitall.implie.example of specifications for expected NER types is in filter-by-tag-ner-types.conf
 */
trait NERFilterByTagNER extends NERFilterable {
  def filterNERsByTag(src: String, relations: List[ImplicitRelation],
                 nerTagsToIgnore: List[String],
                 unexpectedTagsForEntities: Map[String, List[String]]): List[ImplicitRelation] = {

    // Add NER tags for each extraction.
    val taggedNERs = tagNERs(relations, src, nerTagsToIgnore)

    // Filter out NERs that don't match the keyword tag's expected entity type.
    taggedNERs.foreach(extraction => {
      val ners = extraction.ners
      val relation = extraction.relation
      val tagStartIndex = extraction.tag.intervalStart - 1
      val tagEndIndex = extraction.tag.intervalEnd - 1
      //check if tag start is within ner interval or if tag end is within ner interval
      val nersForTag = ners.filter(ner => (ner.beginIndex <= tagStartIndex && tagStartIndex <= ner.endIndex) || 
          (ner.beginIndex <= tagEndIndex && tagEndIndex <= ner.endIndex))
      val unexpectedNERtags = unexpectedTagsForEntities.getOrElse(relation, Nil)

      nersForTag.foreach(ner => if(unexpectedNERtags.contains(ner.ner)) extraction.relation="dropThisRelation")

    })

    // Filter out extractions where an unexpected NER type was found for the tag
    val relationsFiltered = taggedNERs.filter(rel => rel.relation != "dropThisRelation")   

    relationsFiltered
  }
  
  def noFilterTagNERs(src: String, relations: List[ImplicitRelation],
                 nerTagsToIgnore: List[String]):List[ImplicitRelation] = {
    
    // Add NER tags for each extraction.
    val taggedNERs = tagNERs(relations, src, nerTagsToIgnore)
    taggedNERs
  }
  
}
