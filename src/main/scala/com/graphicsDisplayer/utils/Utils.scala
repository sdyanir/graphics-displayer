package com.graphicsDisplayer.utils

object Utils {

  /*
  transform d from [from0,from1] to [to0,to1]
   */
  def linearTransform(from0: Double, from1: Double, to0: Double, to1: Double)(d: Double) =
    if (from0==from1) to1
    else
      d * (to1 - to0) / (from1 - from0) + (from1 * to0 - from0 * to1) / (from1 - from0)
}
