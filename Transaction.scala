package rhein

import java.lang.Comparable
import collection.mutable.PriorityQueue
import scala.collection.mutable.ArrayBuffer
import java.util.concurrent.atomic.AtomicLong

/**
  * Simple handler trait
  * that must implement a run method
  */
trait Handler[T] {
  def run(a: T)
}

/**
  * Helper class to create the priority queue
  *
  * REF! - Based on
  * https://github.com/SodiumFRP/sodium/blob/master
  * /scala/src/main/scala/sodium/Transaction.scala
  * 
  * @param rank
  * @param action
  */
class Entry(var rank: Node, var action: Handler[Transaction])
    extends Comparable[Entry] {
  override def compareTo(other: Entry): Int = {
    rank.compareTo(other.rank)
  }
}
class Transaction() {
  private val pq = new PriorityQueue[Entry]()
  private var last: List[Runnable] = List()

  /**
    * Add a new transaction that is prioritized
    * and runs before everything
    *
    * @param rank
    * @param action
    */
  def prioritized(rank: Node, action: Handler[Transaction]) {
    pq += new Entry(rank, action)
  }

  /**
    * Add a new action that is NOT prioritized
    * and runs last
    *
    * @param action
    */
  def last(action: Runnable) {
    last = last ++ List(action)
  }

  /**
    * Close the transaction
    * Run all actions in pq and last
    * in this specific order
    */
  def close() {
    while (!pq.isEmpty)
      pq.dequeue().action.run(this);

    last.foreach(_.run())
    last = List()
  }
}

object Transaction {

  /**
    * Method to facilitate running the
    * specified code inside a single transaction, with the contained
    * code returning a value of the parameter type A.
    *
    * @param code code to be executed inside the transactional context
    * @return value of the returned code function
    */
  def evaluate[A](code: Transaction => A): A = {
    val trans: Transaction = new Transaction()
    try {
      code(trans)
    } finally {
      trans.close()
    }
  }

  /**
    * Method to facilitate running a piece of code
    * inside a transactional context
    * @param code code to be executed inside the transactional context
    */
  def run(code: Handler[Transaction]) {
    val trans: Transaction = new Transaction()
    try {
      code.run(trans)
    } finally {
      trans.close()
    }
  }
}
