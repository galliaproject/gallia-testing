// ===========================================================================
package object galliatest {

  type WTT[T] = scala.reflect.runtime.universe.WeakTypeTag[T]

  private[galliatest] implicit class GalliaTestingAnything_[A](value: A) { // so as to not import chaining._ everywhere
    def pipe[B](f: A => B)   : B =   f(value)
    def tap [B](f: A => Unit): A = { f(value); value }
    def _p                   : A = { System.out.println(  value ); value }
    def _i(f: A => Any)      : A = { System.out.println(f(value)); value } }
  
}

// ===========================================================================