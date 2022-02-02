package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal{
      val bVal = b()
      (bVal * bVal) - 4 * (a() * c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val negB = Signal(-1 * b())
    val twoA = Signal(2 * a())
    Signal {
      if (delta() < 0) Set()
      else
        Set(
          (negB() + math.sqrt(delta())) / twoA(),
          (negB() - math.sqrt(delta())) / twoA()
        )
    }
  }

