package rhein

/**
  * Event Loop provides a way to create circular dependencies
  * and also helps in creating accumulators
  *
  */
class EventLoop[T] extends EventWithSend[T] {

  private var ea_out: Option[Event[T]] = None

  def loop(initStream: Event[T]) {
    if (ea_out.isDefined)
      throw new RuntimeException("StreamLoop looped more than once")
    ea_out = Some(initStream)
    addCleanup(initStream.listen(this.node, new TransactionHandler[T]() {
      override def run(trans: Transaction, a: T) {
        EventLoop.this.send(trans, a)
      }
    }))
  }
}
