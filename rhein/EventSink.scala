package rhein
import scala.collection.mutable.ListBuffer

class EventSink[T] extends Event[T] {

  /**
    * After creating the dependencies,
    * loop with this function.
    * @param a
    */
  def send(a: T) {
    Transaction.evaluate((trans: Transaction) => { send(trans, a) })
  }

  /**
    * Get all listener actions and send
    * this payload down the pipe
    *
    * @param trans
    * @param a
    */
  def send(trans: Transaction, a: T) {
    try {
      this.listeners
        .clone()
        .asInstanceOf[ListBuffer[TransactionHandler[T]]]
        .foreach(_.run(trans, a))
    } catch {
      case t: Throwable => t.printStackTrace()
    }
  }

}
