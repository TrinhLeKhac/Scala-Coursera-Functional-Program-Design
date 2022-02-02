package example

/**
 * Each signals s(Expr) maintains:
 * current value(s())
 * current expression that defines signal(Expr)
 * set of observers: other signals depend on its value
 *
 * signal changes, all of its observers re-evaluate
 *
 * evaluate signal-valued expression s() => adding caller to the observers of sig
 * signal sig's value changes, all observing signals re-evaluate, set sig.observers is clear
 *
 * examples:
 * s1 = s2 + s3
 * s2 and s3 is s1.observers
 * when we change formula
 * s1 = s2 + s3 * s4
 *
 * */
trait Signal[+T]:
  /**
   * s is a signal
   * s() is current values of signal
   * */
  def apply(): Signal.Observed[T]

object Signal:
  /**
   * observers
   * */
  opaque type Observer = AbstractSignal[?]
  type Observed[T] = Observer ?=> T
  def caller(using o: Observer) = o

  abstract class AbstractSignal[+T] extends Signal[T]:
    private var currentValue: T = _
    private var observers: Set[Observer] = Set()

    protected def eval: Observed[T] // Observer?=> T, we need to implicit pass Observer to eval method
    /**
     * The signal value is evaluated using computeValue()
     * on initialization
     * when an observed signal changes its value
     * */
    protected def computeValue(): Unit =
      val newValue = eval(using this)
      val observeChange = (observers.nonEmpty) && (newValue != currentValue)
      currentValue = newValue
      if(observeChange)
        val obs = observers
        observers = Set()
        obs.foreach(_.computeValue())

    def apply(): Observed[T] =
      observers += caller
      assert(!caller.observers.contains(this), "cyclic signal definition")
      currentValue

  end AbstractSignal

  /**
   * Signal.apply(expr) = Signal(expr) return a signal
   * */
  def apply[T](expr: Observed[T]): Signal[T] = new AbstractSignal[T]:
    val eval = expr
    computeValue()

  /**
   * in Signal object define class Var for variable signals
   * */
  class Var[T](expr: Observed[T]) extends AbstractSignal[T]:
    protected var eval = expr
    computeValue()

    def update(expr: Observed[T]): Unit =
      eval = expr
      computeValue()
  end Var

  given noObserver: Observer = new AbstractSignal[Nothing]:
    override def eval = ???
    override def computeValue() = ()

end Signal




