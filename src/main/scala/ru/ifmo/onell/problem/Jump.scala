package ru.ifmo.onell.problem

class Jump(problemSize: Int, k: Int) extends OneMax(problemSize) {
  private[this] def convert(f: Int): Int = if (f == problemSize || f <= problemSize - k) f else -f
  override def compare(lhs: Int, rhs: Int): Int = convert(lhs) - convert(rhs)
}
