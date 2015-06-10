package edu.knowitall.implie.extractor

import edu.knowitall.tool.chunk.ChunkedToken

/**
 * Simple relation filtering functions that don't depend on another system
 * such as NER tagging or WordNet.
 */
trait BasicFilterFunctions {
  def getTokens(line: String): Seq[ChunkedToken]

  val daysOfWeek = List("monday", "tuesday", "wednesday", "thursday", "friday",
    "saturday", "sunday")

  val punctPostags =
    List("#", "$", ".", ",", ":", "(", ")", "\"", "\'", "\'\'", "`", "``")

  private def firstNonPunct(src: String, relation: ImplicitRelation): Int = {
    val tokens = getTokens(src)

    for (i <- relation.np.beginWordIndex until relation.np.endWordIndex) {
      if (!punctPostags.contains(tokens(i))) {
        return i
      }
    }
    -1
  }

  private def lastNonPunct(src: String, relation: ImplicitRelation): Int = {
    val tokens = getTokens(src)

    for (i <- (relation.np.endWordIndex - 1) to relation.np.beginWordIndex by -1) {
      if (!punctPostags.contains(tokens(i))) {
        return i
      }
    }
    -1
  }

  def filterNNOfEntities(src: String, relations: List[ImplicitRelation]) = {
    val tokens = getTokens(src)

    relations.filter(r => {
      val symbolIndex = firstNonPunct(src, r)
      val symbol = tokens(symbolIndex).postagSymbol.name

      // Can add NNP if we want to filter proper nouns as well.
      // Filter out if entity is 'NN/NNS of ...'
      symbolIndex != -1 &&
        ((!symbol.equals("NN") && !symbol.equals("NNS")) ||
          !tokens(symbolIndex + 1).string.equals("of"))
    })

  }

  def filterDaysOfWeek(src: String, relations: List[ImplicitRelation]) = {
    val tokens = getTokens(src)

    relations.filter(r => {
      val firstSymIndex = firstNonPunct(src, r)
      val lastSymIndex = lastNonPunct(src, r)

      // Filter out any that have a day of the week as the first or last token.
      firstSymIndex != -1 && lastSymIndex != -1 &&
        (!daysOfWeek.contains(tokens(firstSymIndex).string.toLowerCase) &&
          !daysOfWeek.contains(tokens(lastSymIndex).string.toLowerCase))
    })
  }

  // This checks by the entire sentence, rather than the entity...
  // So it either returns Nil or an unchanged list.
  def filterStrangeDateFormats(src: String, relations: List[ImplicitRelation]) = {
    val indicators = List("UTC", "CNA")
    val tokens = getTokens(src)

    val containsIndicator = tokens.foldLeft(false)((acc, cur) => {
      indicators.contains(cur.string) || acc
    })

    if (containsIndicator) {
      Nil
    } else {
      relations
    }
  }

  def filterVsEntities(src: String, relations: List[ImplicitRelation]) = {
    val tokens = getTokens(src)

    val vs = List("vs", "vs.", "v")

    relations.filter(r => {
      var containsVs = false
      for (i <- r.np.beginWordIndex until r.np.endWordIndex) {
        if (vs.contains(tokens(i).string.toLowerCase)) {
          containsVs = true
        }
      }
      !containsVs
    })
  }

  def filterTagIsEntity(src: String, relations: List[ImplicitRelation]) = {
    relations.filter(r =>
      !r.np.string.replaceAll("[\\p{Punct}]", "").toLowerCase.equals(r.tag.text.toLowerCase)
    )
  }

  // NOTE: currently unused.
  def filterTooManyCommas(src: String, relations: List[ImplicitRelation]) = {
    relations.filter(r =>
      r.np.string.count(char => char == ',') > 2
    )
  }
}
