// ===========================================================================
package object galliatest {
  
  private[galliatest] implicit class GalliaAnything_[A](u: A) { // so as to not import chaining._ everywhere
    def pipe[B](f: A => B)   : B    = f(u)
    def tap [B](f: A => Unit): Unit = f(u)
  }
  
}

// ===========================================================================