package ru.ifmo.onell.distribution

import java.util.Random

import scala.language.implicitConversions

/**
  * This trait represents a contiguous integer distribution.
  *
  * It provides methods `minValue` and `maxValue`, which define a range for integers this distribution may provide.
  * Sampling is done using the `sample(rng: Random)` method, which returns one `Int` at a time.
  *
  * The design constraint is that each of the values between `minValue` and `maxValue` may be produced
  * with nonzero mathematical probability (but it may have a zero computer probability, for instance if
  * the mathematical probability cannot be represented as a nonzero `Double`, or if the random number generator
  * that is passed to `sample` is especially weak). So if `minValue > maxValue`, each call to `sample` fails.
  */
trait IntegerDistribution {
  /**
    * Samples an `Int` from the distribution.
    *
    * This value will be at least `minValue` and at most `maxValue`
    * Note that if `minValue > maxValue`, in which case `isEmpty` returns `true`, this method throws an exception.
    *
    * @param rng the random number generator to use.
    * @return the just sampled `Int`.
    */
  def sample(rng: Random): Int

  /**
    * Returns the minimum value (inclusively) that can be sampled from this distribution.
    *
    * If `minValue > maxValue`, then no sampling succeeds,
    * otherwise `minValue` has a nonzero mathematical probability of being sampled.
    * Be aware that, in some cases, this value is extremely unlikely to be sampled.
    * In this case, which may depend on the random number generator,
    * there may even be a zero computer probability of generating `minValue`.
    *
    * @return the minimum value that can be sampled from this distribution.
    */
  def minValue: Int

  /**
    * Returns the maximum value (inclusively) that can be sampled from this distribution.
    *
    * If `minValue > maxValue`, then no sampling succeeds,
    * otherwise `maxValue` has a nonzero mathematical probability of being sampled.
    * Be aware that, in some cases, this value is extremely unlikely to be sampled.
    * In this case, which may depend on the random number generator,
    * there may even be a zero computer probability of generating `maxValue`.
    *
    * @return the maximum value that can be sampled from this distribution.
    */
  def maxValue: Int

  /**
    * Returns whether it is true that this distribution cannot sample any value.
    *
    * Generally, `isEmpty` is equivalent to `minValue > maxValue`.
    * When `isEmpty` returns `true`, any call to `sample` fails.
    *
    * @return whether it is true that this distribution cannot sample any value.
    */
  def isEmpty: Boolean = minValue > maxValue

  /**
    * Returns whether this distribution can actually sample any value.
    *
    * Generally, `nonEmpty` is equivalent to `minValue <= maxValue`.
    * When `nonEmpty` returns `false`, any call to `sample` fails.
    *
    * @return whether this distribution can actually sample any value.
    */
  def nonEmpty: Boolean = minValue <= maxValue
}

object IntegerDistribution {
  private[this] val cache = Array.tabulate(10)(i => new Constant(i))

  def empty: IntegerDistribution = Empty
  def constant(value: Int): IntegerDistribution =
    if (value >= 0 && value < cache.length)
      cache(value)
    else new Constant(value)

  implicit def constant2distribution(value: Int): IntegerDistribution = constant(value)

  private final class Constant(value: Int) extends IntegerDistribution {
    override def sample(rng: Random): Int = value
    override def minValue: Int = value
    override def maxValue: Int = value
  }

  private final object Empty extends IntegerDistribution {
    override def sample(rng: Random): Int = throw new IllegalStateException("Attempt to sample an empty distribution")
    override def minValue: Int = Int.MaxValue
    override def maxValue: Int = Int.MinValue
  }
}
