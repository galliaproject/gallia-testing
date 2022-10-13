// ===========================================================================
package object galliatest {
  type WTT[T] = scala.reflect.runtime.universe.WeakTypeTag[T]

  // ---------------------------------------------------------------------------
  private[galliatest] implicit class GalliaTestingAnything_[A](value: A) { // so as to not import chaining._ everywhere
    def pipe[B](f: A => B)   : B =   f(value)
    def pype[B](f: A => B)   : B =   f(value)
    def tap    (f: A => Unit): A = { f(value); value }

    def p___                  : A = new aptus.Anything_(value).p__
    def p_                    : A = new aptus.Anything_(value).p
    def i_(f: A => Any)       : A = new aptus.Anything_(value).i(f)

    def __p                   : A = new aptus.Anything_(value).p__
    def  _p                   : A = new aptus.Anything_(value).p
    def  _i(f: A => Any)      : A = new aptus.Anything_(value).i(f) }

    // ---------------------------------------------------------------------------
    private[galliatest] type ByteBuffer    = java.nio.ByteBuffer // note: use ByteBuffer.wrap(_: Array[Byte])
    private[galliatest] type Temporal = java.time.temporal.Temporal

      private[galliatest] type LocalTime      = java.time. LocalTime
      private[galliatest] type LocalDate      = java.time. LocalDate

      private[galliatest] type LocalDateTime  = java.time. LocalDateTime
      private[galliatest] type OffsetDateTime = java.time.OffsetDateTime
      private[galliatest] type ZonedDateTime  = java.time. ZonedDateTime

      private[galliatest] type Instant        = java.time.Instant
}

// ===========================================================================