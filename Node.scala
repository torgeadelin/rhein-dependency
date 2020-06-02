package rhein

import scala.collection.mutable.HashSet

/**
  * Node class that is used to implement
  * the ranking system
  *
  * Used to make sure events are executed
  * in proper order
  *
  * REF! - Based on:
  * https://github.com/SodiumFRP/sodium/blob/master/scala/src/main/scala/sodium/Node.scala
  *
  * @param rank
  */
class Node(var rank: Long) extends Comparable[Node] {
  import Node._
  val listeners: HashSet[Node] = HashSet()

  /**
    * Links one node to another
    * Creates a new connection in the dependecy
    * graph
    *
    * @param target
    * @return
    */
  def linkTo(target: Node): Boolean =
    if (target == NullNode) {
      false
    } else {
      val changed = target.ensureBiggerThan(rank, Set())
      listeners.add(target)
      changed
    }

  /**
    * Breakes a connection between two nodes
    * Removes a dependency
    *
    * @param target
    */
  def unlinkTo(target: Node) {
    if (target != NullNode)
      listeners.remove(target)
  }

  /**
    * Ensures that the nodes added inside
    * the dependency graph are ordered by the ranks
    *
    * @param limit
    * @param visited
    * @return
    */
  private def ensureBiggerThan(limit: Long, visited: Set[Node]): Boolean = {
    if (rank > limit || visited.contains(this)) {
      false
    } else {
      val accVisited = Set(this) ++ visited
      rank = limit + 1
      listeners.forall(_.ensureBiggerThan(rank, visited))
    }
  }

  /**
    * Helper method to compare two
    * nodes by rank
    *
    * @param o
    * @return
    */
  override def compareTo(o: Node): Int =
    if (rank < o.rank) -1
    else if (rank > o.rank) 1
    else 0
}

/**
  * Companion Object
  */
object Node {
  // Null node, used when implementing the
  // listen method for developer
  val NullNode = new Node(Long.MaxValue)
}
