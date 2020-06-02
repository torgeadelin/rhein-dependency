package rhein

/**
  * Behaviour - time varying value
  * Differentiates from an Event by always having a value => continuous
  * @param event
  * @param value
  */
class Behaviour[T](var event: Event[T], var value: Option[T]) {
  var valueUpdate: Option[T] = None
  var cleanup: Option[Listener] = None

  // Auxiliary constructor
  def this(value: Option[T]) {
    this(new Event[T](), value)
  }

  // Creates the behaviour dependency
  // Using a Transaction
  // Listens for changes on the injected event stream
  Transaction.evaluate((trans1: Transaction) => {
    this.cleanup = Some(
      event.listen(
        Node.NullNode,
        trans1,
        (trans2: Transaction, a: T) => {
          // make sure the updates happen at the end of a transaction
          // and before any new one
          if (Behaviour.this.valueUpdate.isEmpty) {
            trans2.last(new Runnable() {
              def run() {
                Behaviour.this.value = valueUpdate
                Behaviour.this.valueUpdate = None
              }
            })
          }
          this.valueUpdate = Some(a)
        }
      )
    )
  })

  /**
    * Sample the current value without a transaction
    *
    * @return the current value of the behaviour
    */
  def sampleNoTrans(): T = value.get

  /**
    * Returns the newst value of the behaviour
    *
    * @return
    */
  def newValue(): T = {
    valueUpdate.getOrElse(sampleNoTrans)
  }

  /**
    * Returns an event with all changes that have been made to this
    * Behaviour. Gives you the discrete updates to a behaviour (effectively the inverse of hold())
    * @return
    */
  def changes(): Event[T] = {
    event
  }

  /**
    * Map primitive
    *
    * @param f transformation function
    * @return
    */
  final def map[B](f: T => B): Behaviour[B] = {
    changes().map(f).hold(f.apply(sampleNoTrans()))
  }

  /**
    * Lift primitive
    *
    * @param b to be combined with
    * @param f combination function
    * @return
    */
  final def lift[B, C](b: Behaviour[B], f: (T, B) => C): Behaviour[C] = {
    def ffa(aa: T)(bb: B) = f(aa, bb)
    Behaviour.apply(map(ffa), b)
  }

  override def finalize() = {
    cleanup.get.unlisten
  }

}

/**
  * Companion Object
  */
object Behaviour {

  /**
    * Helper constructor for the lift primitive
    * Combines two behaviours together
    *
    * @param bf
    * @param ba
    * @return
    */
  def apply[T, B](bf: Behaviour[T => B], ba: Behaviour[T]): Behaviour[B] = {
    val out = new EventSink[B]()
    var fired = false
    def h(trans: Transaction) {
      if (!fired) {
        fired = true
        trans.prioritized(out.node, { trans2 =>
          out.send(trans2, bf.newValue().apply(ba.newValue()))
          fired = false
        })
      }
    }
    val l1 = bf
      .changes()
      .listen(out.node, new TransactionHandler[T => B]() {
        def run(trans: Transaction, f: T => B) {
          h(trans)
        }
      })
    val l2 = ba
      .changes()
      .listen(out.node, new TransactionHandler[T]() {
        def run(trans: Transaction, a: T) {
          h(trans)
        }
      })
    out
      .addCleanup(l1)
      .addCleanup(l2)
      .hold(bf.sampleNoTrans().apply(ba.sampleNoTrans()))
  }
}
