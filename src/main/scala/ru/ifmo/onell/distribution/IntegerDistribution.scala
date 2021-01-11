package ru.ifmo.onell.distribution

import java.util.Random

import scala.annotation.tailrec
import scala.language.implicitConversions

/**
  * This trait represents a contiguous integer distribution.
  *
  * It provides methods `minValue` and `maxValue`, which define a range for integers this distribution may provide.
  * The design constraint is that each of these values may be produced with nonzero probability, so, among others,
  * `minValue <= maxValue`. Sampling is done using the `sample(rng: Random)` method, which returns one `Int` at a time.
  *
  * This trait also provides basic composition methods that can transform the distributions in the ways that are
  * most common in the intended usage domain.
  */
trait IntegerDistribution { self =>
  import IntegerDistribution._

  /**
    * Samples an `Int` from the distribution. This value will be at least `minValue` and at most `maxValue`
    * @param rng the random number generator to use.
    * @return the just sampled `Int`.
    */
  def sample(rng: Random): Int

  /**
    * Returns the minimum value (inclusively) that can be sampled from this distribution.
    * Be aware that, in some cases, this value is extremely unlikely to be sampled.
    * @return the minimum value that can be sampled from this distribution.
    */
  def minValue: Int

  /**
    * Returns the maximum value (inclusively) that can be sampled from this distribution.
    * Be aware that, in some cases, this value is extremely unlikely to be sampled.
    * @return the maximum value that can be sampled from this distribution.
    */
  def maxValue: Int

  /**
    * Returns a maximum of this distribution and a constant `limit`.
    *
    * In the general case, the new distribution will query `this` and return the maximum of that value
    * and `limit`. In a special case, if `limit` is greater than or equal to the maximum value of `this`,
    * a constant distribution is returned that does not sample `this`.
    *
    * @param limit the lower limit to be imposed on the distribution.
    * @return the new distribution that returns `limit` whenever a smaller value is sampled from the original distribution.
    */
  def max(limit: Int): IntegerDistribution = {
    if (limit <= self.minValue)
      this
    else if (limit >= self.maxValue)
      limit
    else new IntegerDistribution {
      override def sample(rng: Random): Int = math.max(self.sample(rng), limit)
      override val minValue: Int = math.max(self.minValue, limit)
      override val maxValue: Int = self.maxValue
    }
  }

  /**
    * Returns a distribution that is `this` distribution plus a given constant `value`.
    * @param value the constant to add.
    * @return the new distribution that is always by `value` greater than `this`.
    */
  def plus(value: Int): IntegerDistribution = new Plus(this, value)

  /**
    * Returns a distribution that negates each value returned by `this`.
    * @return the negated distribution.
    */
  def negate(): IntegerDistribution = new IntegerDistribution {
    override def sample(rng: Random): Int = -self.sample(rng)
    override def minValue: Int = -self.maxValue
    override def maxValue: Int = -self.minValue
  }

  /**
    * Returns a distribution that continues querying `this` until the returned value is `limit` or above.
    * In a special case that only one value, out of these that can be returned by `this`, satisfies the predicate,
    * a constant distribution is returned that never queries `this`. In another special case that none of the values
    * that can be returned by `this` satisfy the predicate, this method throws an `IllegalArgumentException`.
    *
    * @param limit the lower limit, below which the distribution will be queried again.
    * @return the new distribution that resamples the original distribution each time it returns a value less than `limit`.
    */
  def resampleIfBelow(limit: Int): IntegerDistribution = {
    if (limit <= self.minValue)
      this
    else if (limit == self.maxValue)
      maxValue
    else if (limit > self.maxValue)
      throw new IllegalArgumentException(s"The limit, $limit, is above the maximum value of this distribution, ${self.maxValue}")
    else new IntegerDistribution {
      @tailrec
      override final def sample(rng: Random): Int = {
        val v = self.sample(rng)
        if (v >= limit) v else sample(rng)
      }
      override val minValue: Int = math.max(limit, self.minValue)
      override val maxValue: Int = self.maxValue
    }
  }
}

object IntegerDistribution {
  private[this] val cache = Array.tabulate(10)(i => new Constant(i))

  def constant(value: Int): IntegerDistribution =
    if (value >= 0 && value < cache.length)
      cache(value)
    else new Constant(value)

  implicit def constant2distribution(value: Int): IntegerDistribution = constant(value)

  private final class Constant(value: Int) extends IntegerDistribution {
    override def sample(rng: Random): Int = value
    override def minValue: Int = value
    override def maxValue: Int = value
    override def plus(value: Int): IntegerDistribution = this.value + value
    override def negate(): IntegerDistribution = -this.value
  }

  private final class Plus(base: IntegerDistribution, value: Int) extends IntegerDistribution {
    override def sample(rng: Random): Int = value + base.sample(rng)
    override def minValue: Int = value + base.minValue
    override def maxValue: Int = value + base.maxValue
    override def plus(value: Int): IntegerDistribution = new Plus(base, this.value + value)
    override def negate(): IntegerDistribution = new Plus(base.negate(), -value)
  }
}
