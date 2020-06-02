package rhein

import scala.collection.mutable.ListBuffer
// Listener trait
// must implement a unlisten method

/**
  * Listener trait
  */
trait Listener {
  def unlisten()
}

// TransactionHandler[A]
// must implement a run method

/**
  * Transaction Handler is used to implement listeners
  */
trait TransactionHandler[A] {
  def run(trans: Transaction, a: A)
}

// Also known as Stream
// Stream of events that fire at discrete times

/**
  * Event, also known as Stream in other FRP systems,
  * is a stream of events that fire at discrete points
  * in time
  *
  */
class Event[T]() {
  // List with listeners on this event
  protected var listeners = new ListBuffer[TransactionHandler[T]]()
  // Collects all listeners that need to be removed when this event gets killed
  protected var finalizers = new ListBuffer[Listener]()
  // Uset to identify each event
  var node: Node = new Node(0L);

  protected val firings = ListBuffer[T]()

  /**
    * Listener implementation. When creating a new listener on an event,
    * it returns an instance of this class and then the
    * listener can be closed/killed
    *
    * @param event
    * @param action
    * @param target
    */
  final class ListenerImplementation[T](
      event: Event[T],
      action: TransactionHandler[T],
      target: Node
  ) extends Listener {

    /**
      * Unlisten method that breaks the dependency
      * relation
      */
    def unlisten() = {
      event.listeners -= action
      event.node.unlinkTo(target)
    }

    override protected def finalize() = {
      unlisten()
    }
  }

  /**
    * Listen for firings of this event. The returned Listener has an unlisten()
    * method to cause the listener to be removed. This is the observer pattern.
    * @param action
    * @return
    */
  def listen(action: Handler[T]): Listener = {
    listen(Node.NullNode, (trans: Transaction, a: T) => { action.run(a) })
  }

  /**
    * Listeners used for implementing primitives
    * @param target
    * @param action
    * @return
    */
  def listen(target: Node, action: TransactionHandler[T]): Listener = {
    Transaction.evaluate((trans: Transaction) => listen(target, trans, action))
  }

  /**
    * The final listener method of the whole listen chain
    * that creates dependencies and adds actions to the
    * list of actions of this event
    *
    *
    * @param target
    * @param trans
    * @param action
    * @return listener implementation that can be used to unlisten
    */
  def listen(
      target: Node,
      trans: Transaction,
      action: TransactionHandler[T]
  ): Listener = {
    node.linkTo(target)
    listeners += action
    new ListenerImplementation[T](this, action, target)
  }

  /**
    * Map Primitive
    *
    * @param f transformation function
    * @return
    */
  def map[B](f: T => B): Event[B] = {
    val out: EventSink[B] = new EventSink[B]();
    val l: Listener = listen(out.node, (trans: Transaction, a: T) => {
      out.send(trans, f.apply(a));
    })
    out.addCleanup(l)
  }

  /**
    * Hold primitive
    *
    * @param initValue required as behaviours always have a value
    * @return
    */
  final def hold(initValue: T): Behaviour[T] = {
    Transaction.evaluate(trans => new Behaviour[T](this, Some(initValue)))
  }

  /**
    * Filter primitive
    *
    * @param f filtering function
    * @return
    */
  def filter(f: T => Boolean): Event[T] = {
    val out = new EventSink[T]()
    val l = listen(out.node, (trans: Transaction, a: T) => {
      if (f(a)) out.send(trans, a)
    })
    out.addCleanup(l)
  }

  /**
    * Snapshot primitive
    *
    * @param b behaviour being snapshoted
    * @param f combination function
    * @return
    */
  def snapshot[B, C](b: Behaviour[B], f: (T, B) => C): Event[C] = {
    val out = new EventSink[C]()
    val l: Listener = listen(out.node, new TransactionHandler[T]() {
      def run(trans: Transaction, a: T) {
        out.send(trans, f(a, b.sampleNoTrans()))
      }
    })

    out.addCleanup(l)
  }

  /**
    * Helper that adds listeners to
    * the list of finalizers of this eventg
    *
    * @param l
    * @return
    */
  def addCleanup(l: Listener): Event[T] = {
    finalizers += l
    this
  }

  /**
    * Removes the listeners while this event is killed
    */
  override def finalize() {
    finalizers.foreach(_.unlisten)
  }
}

/**
  * Companion Object
  */
object Event {

  /**
    * Merge primitive
    *
    * @param ea
    * @param eb
    * @return
    */
  def merge[T](ea: Event[T], eb: Event[T]): Event[T] = {
    val out: EventSink[T] = new EventSink[T]()
    val h = new TransactionHandler[T]() {
      def run(trans: Transaction, a: T) {
        out.send(trans, a)
      }
    }

    val l1 = ea.listen(out.node, h)
    val l2 = eb.listen(
      out.node,
      new TransactionHandler[T]() {
        def run(trans1: Transaction, a: T) {
          trans1.prioritized(out.node, new Handler[Transaction]() {
            def run(trans2: Transaction) {
              out.send(trans2, a)
            }
          })
        }
      }
    )
    out.addCleanup(l1).addCleanup(l2)
  }

  /**
    * Special event interval, that emits at a given rate
    *
    * @param delay
    * @param period
    * @return
    */
  def interval(delay: Long, period: Long): Event[Unit] = {
    val out: EventSink[Unit] = new EventSink()
    val t = new java.util.Timer()

    val task = new java.util.TimerTask {
      def run() = out.send(Unit)
    }
    t.schedule(task, delay, period)

    out
  }
}
