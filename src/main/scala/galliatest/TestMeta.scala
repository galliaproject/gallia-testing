package galliatest

// ===========================================================================
object TestMeta {
  case class Foo(a: String, A: String)
  
  // ---------------------------------------------------------------------------
  case class Default01DataClass(f:     String , g: Int)
  case class Default02DataClass(f: Seq[String], g: Int)
  case class InvalidDataClass1 (f:     String , g: java.io.File)
       class InvalidDataClass2 (f:     String , g: Int)
  
}

// ===========================================================================
