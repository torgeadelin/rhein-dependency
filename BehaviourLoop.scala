package rhein

/**
  * Behaviour Loop provides a way to create circular dependencies
  * and also helps in creating accumulators
  *
  */
final class BehaviourLoop[T] extends Behaviour[T](new EventLoop[T](), None) {

  /**
    * After creating the dependencies,
    * loop with this function.
    *
    * @param a_out
    */
  def loop(a_out: Behaviour[T]) {
    event match {
      case s: EventLoop[T] => s.loop(a_out.changes())
      case _               =>
    }
    value = Some(a_out.sampleNoTrans)
  }

  override def sampleNoTrans(): T = {
    if (value.isEmpty)
      throw new RuntimeException("CellLoop sampled before it was looped")
    value.get
  }
}
