package example

trait Simulation:

  /**
   * define Action
   * define Event
   * define Agenda
   * handle insert, remove events in agenda
   * method run to demo
   */
  type Action = () => Unit // Action, take no parameters and return Unit
  private case class Event(time: Int, action: Action)
  private type Agenda = List[Event]
  private var agenda: Agenda = List()
  private var curtime = 0

  private def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case first::rest if first.time <= item.time => first::insert(rest, item)
    case _ => item::ag
  }

  def currentTime = curtime

  def afterDelay(delay: Int)(block: => Unit): Unit =
    var item = Event(curtime + delay, () => block)
    agenda = insert(agenda, item)

  private def loop(): Unit = agenda match {
    case first :: rest => {
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    }
    case Nil => ()
  }

  def run(): Unit =
    afterDelay(0) {
      println(s"***simulation started, time = $currentTime***")
    }
    loop()

end Simulation

/**
 * define Delay in logic Gate
 * define: InverterDelay, AndGateDelay, OrGateDelay
 * */
trait Delays:
  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int
end Delays

/**
 * Define get, set properties in type Wire
 * method addAction in Wire(save action in wires)
 * */
class Wire extends Simulation:

  def InverterDelay: Int = 5
  def AndGateDelay: Int = 3
  def OrGateDelay: Int = 2

  private var sigVal =false
  private var actions: List[Action] = List()

  def getSignal(): Boolean = sigVal
  def setSignal(s: Boolean): Unit =
    if(s != sigVal) {
      sigVal = s
      actions.foreach(_())
    }
  def addAction(a: Action): Unit = {
    actions = a::actions
    a()
  }
end Wire

/**
 * define logic gate
 * inverter
 * andGate
 * orGate
 * probe
 * */
trait Gates extends Simulation with Delays:

  def inverter(input: Wire, output: Wire): Unit =
    def invertAction(): Unit =
      val inputSig = input.getSignal()
      afterDelay(InverterDelay) {
        output.setSignal(!inputSig)
      }
    input.addAction(invertAction)

  def andGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def andAction(): Unit =
      val in1Sig = in1.getSignal()
      val in2Sig = in2.getSignal()
      afterDelay(AndGateDelay) {
        output.setSignal(in1Sig && in2Sig)
      }
    in1.addAction(andAction)
    in2.addAction(andAction)
  }

  def orGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def orAction(): Unit =
      val in1Sig = in1.getSignal()
      val in2Sig = in2.getSignal()
      afterDelay(OrGateDelay) {
        output.setSignal(in1Sig || in2Sig)
      }
    in1.addAction(orAction)
    in2.addAction(orAction)
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name ${currentTime} value = ${wire.getSignal()}")
    }
    wire.addAction(probeAction)
  }

end Gates

/**
 * Circuit by combine wires with logic in Gates
 * 2 types of circuit:
 * halfAdder
 * fullAdder
 * */
trait Circuits extends Wire with Gates:
  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit =
    val d = Wire()
    val e = Wire()
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e , s)

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit =
    val s = Wire()
    val c1 = Wire()
    val c2 = Wire()
    halfAdder(a, cin, s, c1)
    halfAdder(b, s, sum, c2)
    orGate(c1, c2, cout)
end Circuits

object sim extends Circuits:
  def main(args: Array[String]): Unit = {
    val input1, input2, sum, carry = Wire()
    probe("sum", sum)
    probe("carry", carry)
    halfAdder(input1, input2, sum, carry)
    input1.setSignal(true)
    run()
    input2.setSignal(true)
    run()
  }

/**
 * state and assignment alows us to formulate programs in elegant way
 * but we lose referential transparency
 * Example: a system is represented by a mutable list of actions
 * call the actions => change the state of objects
 *
 * choose between functional programming and imperative programming depend on the situation
 * */






