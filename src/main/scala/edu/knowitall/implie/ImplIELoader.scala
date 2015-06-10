package edu.knowitall.implie

import edu.knowitall.implie.extractor.ImplIE

/**
 * Object to load default ImplIE systems from.
 * Memoizes the generated ImplIE classes so they don't get reloaded.
 * Just uses the same instance.
 */
object ImplIELoader {
  val implieMemo = scala.collection.mutable.Map[String, ImplIE]()
  val highRecallKey = "high-recall"
  val highPrecisionKey = "high-precision"

  def defaultImplIE = highRecallImplIE
  def fastImplIE = highPrecisionImplIE

  def highRecallImplIE: HighRecallImplIE = {
    implieMemo.get(highRecallKey) match {
      case Some(result) => result.asInstanceOf[HighRecallImplIE]
      case None =>
        val highRecallImplIE =
          new HighRecallImplIE(TaggerLoader.highRecallTagger)
        implieMemo.put(highRecallKey, highRecallImplIE)
        highRecallImplIE
    }
  }

  def highPrecisionImplIE: HighPrecisionImplIE = {
    implieMemo.get(highPrecisionKey) match {
      case Some(result) => result.asInstanceOf[HighPrecisionImplIE]
      case None =>
        val highPrecisionImplIE =
          new HighPrecisionImplIE(TaggerLoader.highPrecisionTagger)
        implieMemo.put(highPrecisionKey, highPrecisionImplIE)
        highPrecisionImplIE
    }
  }
}
