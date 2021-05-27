package galliatest

// ===========================================================================
object TestMeta {
  case class Foo(a: String, A: String)

  case class Baz1(a: String, A: String, q:               Qux  )
  case class Baz2(a: String, A: String, q:        Option[Qux] )
  case class Baz3(a: String, A: String, q:        Seq   [Qux] ) { require(q         .nonEmpty , this) }
  case class Baz4(a: String, A: String, q: Option[Seq   [Qux]]) { require(q.forall(_.nonEmpty), this) }
  
    case class Qux(i: Int)
  
  // ---------------------------------------------------------------------------
  case class Default01DataClass(f:     String , g: Int)
  case class Default02DataClass(f: Seq[String], g: Int)
  case class InvalidDataClass1 (f:     String , g: java.io.File)
       class InvalidDataClass2 (f:     String , g: Int)

}

// ===========================================================================
