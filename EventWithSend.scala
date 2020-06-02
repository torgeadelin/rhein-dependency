package rhein

/**
  * Modified version of send used to implement
  * event loops.
  */
class EventWithSend[T] extends Event[T] {

  /**
    * Send method used to trigger events
    * associated with a pyload
    * inside a transaction
    *
    * @param trans
    * @param a
    */
  def send(trans: Transaction, a: T) {
    if (firings.isEmpty)
      trans.last(new Runnable() {
        def run() { firings.clear() }
      })
    firings += a

    try {
      listeners.clone.foreach(_.run(trans, a))
    } catch {
      case t: Throwable => t.printStackTrace()
    }
  }

}
