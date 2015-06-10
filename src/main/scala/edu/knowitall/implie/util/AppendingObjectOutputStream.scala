package edu.knowitall.implie.util

import java.io.{ObjectOutputStream, OutputStream}

/**
 * Created by Gene on 1/11/2015.
 */
class AppendingObjectOutputStream(out: OutputStream) extends ObjectOutputStream(out) {
  override protected def writeStreamHeader() {
    // do not write a header, but reset:
    // this line added after another question
    // showed a problem with the original
    reset()
  }
}
