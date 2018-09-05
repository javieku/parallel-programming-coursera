package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    def balance_impl(acc: Int, currentIndex: Int): Boolean = {
      if (currentIndex >= chars.length)
        acc == 0
      else if (acc < 0)
        return false
      else if (chars.charAt(currentIndex).equals('('))
        balance_impl(acc + 1, currentIndex + 1)
      else if (chars.charAt(currentIndex).equals(')'))
        balance_impl(acc - 1, currentIndex + 1)
      else
        balance_impl(acc, currentIndex + 1)
    }

    balance_impl(0, 0)
  }

  /** Returns `true` if the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, open: Int, close: Int): (Int, Int) = {

      if (idx >= until)
        return (open, close)
      else if (chars.charAt(idx).equals('('))
        traverse(idx + 1, until, open + 1, close)
      else if (chars.charAt(idx).equals(')'))
        if (open > 0)
          traverse(idx + 1, until, open - 1, close)
        else
          traverse(idx + 1, until, open, close - 1)
      else
        traverse(idx + 1, until, open, close)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold)
        traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val ((open1, close1), (open2, close2)) = parallel(reduce(from, mid),
          reduce(mid, until))

        return (open1 + close2 + open2, close1)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
