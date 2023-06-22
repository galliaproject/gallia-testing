// ===========================================================================
package object galliatesting0 {
  type WTT[T] = scala.reflect.runtime.universe.WeakTypeTag[T]

  // ===========================================================================
  private[galliatesting0] implicit class GalliaTestingAnything2_[A](value: A) { // so as to not import chaining._ everywhere
    def pipe[B](f: A => B)   : B =   f(value)
    def pype[B](f: A => B)   : B =   f(value)
    def tap    (f: A => Unit): A = { f(value); value }

    def p___                  : A = new aptus.Anything_(value).p__
    def p_                    : A = new aptus.Anything_(value).p
    def i_(f: A => Any)       : A = new aptus.Anything_(value).i(f)

    def __p                   : A = new aptus.Anything_(value).p__
    def  _p                   : A = new aptus.Anything_(value).p
    def  _i(f: A => Any)      : A = new aptus.Anything_(value).i(f) }

  // ===========================================================================
  private[galliatesting0] type ByteBuffer    = java.nio.ByteBuffer // note: use ByteBuffer.wrap(_: Array[Byte])
  private[galliatesting0] type Temporal = java.time.temporal.Temporal

      private[galliatesting0] type LocalTime      = java.time. LocalTime
      private[galliatesting0] type LocalDate      = java.time. LocalDate

      private[galliatesting0] type LocalDateTime  = java.time. LocalDateTime
      private[galliatesting0] type OffsetDateTime = java.time.OffsetDateTime
      private[galliatesting0] type ZonedDateTime  = java.time. ZonedDateTime

      private[galliatesting0] type Instant        = java.time.Instant
}

// ===========================================================================