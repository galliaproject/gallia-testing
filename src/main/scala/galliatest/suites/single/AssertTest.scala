package galliatest.suites.single

import gallia._

// ===========================================================================
object AssertTest extends gallia.testing.Suite {
  import TestDataO.{
    Default01, Default02,
    Default13m, Default13p, Default14m, Default14p}
  import TestDataS.Default51
  import TestMeta.{Default01DataClass, InvalidDataClass1, InvalidDataClass2, Default02DataClass}
  import vldt._Error

  // ===========================================================================
  override def test() {
    testMiscAsserts(in = Default01)
    testAssertDataUnsafeU()
    testCustoms(inO = Default01, inS = Default51)
    testAssertDataClass(in = Default01)
    testAssertDataU()

    TestDataO.Default13p.noop(_.assertField('f).matches(_.isOpt))
  }

  // ===========================================================================
  private def testMiscAsserts(implicit in: BObj) {
    in .noop(_.assertMeta(_.size == 2))
    in .noop(_.assertIsOne   ('f))
    in .noop(_.assertIsString('f))
    in .noop(_.assertField   ('f).matches(_.isOne))
    in .noop(_.forKey(_.firstKey).thn(_ assertIsOne _))

    in.assertIsString('g).metaError[_Error.ValueTypeAssertionFailure]

    in.noop(_.assertField('g).matches(_.isNumericalType))
  }

  // ===========================================================================
  private def testAssertDataUnsafeU() {
    Default01 .noop(_.assertDataUnsafeU(_.string('f).contains("oo")))
    Default51 .noop(_.assertDataUnsafeU(_.string('f).contains("oo")))
    Default51 .noop(_.assertDataUnsafeZ(_.size < 10))
    Default51        .assertDataUnsafeZ(_.size <  1).dataError[_Error.Runtime.DataUnsafeZAssertionFailure.type]
  }

  // ===========================================================================
  private def testCustoms(inO: BObj, inS: BObjs) {
    inO.customU2U(identity,       _.transformPath('f, _.asInstanceOf[String].toUpperCase) ).check(      bobj('f -> "FOO", 'g -> 1))

    inS.customU2U(identity,       _.transformPath('f, _.asInstanceOf[String].toUpperCase) ).check(bobjs(bobj('f -> "FOO", 'g -> 1), bobj('f -> "FOO2", 'g -> 2)))
    inS.customZ2Z(identity, _.map(_.transformPath('f, _.asInstanceOf[String].toUpperCase))).check(bobjs(bobj('f -> "FOO", 'g -> 1), bobj('f -> "FOO2", 'g -> 2)))
    inS.customZ2Z(identity,       _.transformPath('f, _.asInstanceOf[String].toUpperCase) ).check(bobjs(bobj('f -> "FOO", 'g -> 1), bobj('f -> "FOO2", 'g -> 2)))
    inS.customS2S(identity, _.map(_.transformPath('f, _.asInstanceOf[String].toUpperCase))).check(bobjs(bobj('f -> "FOO", 'g -> 1), bobj('f -> "FOO2", 'g -> 2)))

    // ---------------------------------------------------------------------------
    {
      import aptus._ // for .assert

               inS.noop(_.customZ2Z(identity, _.assert(_.size < 10)))
      util.Try(inS       .customZ2Z(identity, _.assert(_.size <  1)).check(null)).assert(_.isFailure)
    }
  }

  // ===========================================================================
  private def testAssertDataClass(in: BObj) {
    in.noop(_.assertDataClass[Default01DataClass])
    in       .assertDataClass[InvalidDataClass1 ].metaError[_Error.InvalidDataClass]
    in       .assertDataClass[InvalidDataClass2 ].metaError[_Error.InvalidDataClass]
    in       .assertDataClass[Default02DataClass].metaError[_Error.SchemaMismatch]
  }

  // ===========================================================================
  private def testAssertDataU() {
    Default01        .assertDataU(_.string ('g)).using(_         .contains("oo") ).metaError[_Error        .TypeMismatch]
    Default01        .assertDataU(_.string ('f)).using(_         .contains("OO") ).dataError[_Error.Runtime.DataAssertionFailure]
    Default01 .noop(_.assertDataU(_.string ('f)).using(_         .contains("oo")))

    Default02        .assertDataU(_.string ('g)).using(_         .contains("oo") ).metaError[_Error        .TypeMismatch]
    Default02        .assertDataU(_.strings('f)).using(_.forall(_.contains("OO"))).dataError[_Error.Runtime.DataAssertionFailure]
    Default02 .noop(_.assertDataU(_.strings('f)).using(_.forall(_.contains("oo"))))

    Default13p       .assertDataU(_.string ('f)).using(_         .contains("oo") ).metaError[_Error        .TypeMismatch]
    Default13p       .assertDataU(_.string_('f)).using(_.forall(_.contains("OO"))).dataError[_Error.Runtime.DataAssertionFailure]
    Default13p.noop(_.assertDataU(_.string_('f)).using(_.forall(_.contains("oo"))))

    Default13m       .assertDataU(_.string ('f)).using(_         .contains("oo") ).metaError[_Error        .TypeMismatch]
    Default13m.noop(_.assertDataU(_.string_('f)).using(_.forall(_.contains("OO"))))
    Default13m.noop(_.assertDataU(_.string_('f)).using(_.forall(_.contains("oo"))))

    Default14p       .assertDataU(_.string  ('f)).using(_                       .contains("oo") ).metaError[_Error        .TypeMismatch]
    Default14p       .assertDataU(_.strings_('f)).using(_.toSeq.flatten.forall(_.contains("OO"))).dataError[_Error.Runtime.DataAssertionFailure]
    Default14p.noop(_.assertDataU(_.strings_('f)).using(_.toSeq.flatten.forall(_.contains("oo"))))

    Default14m       .assertDataU(_.string  ('f)).using(_                       .contains("oo") ).metaError[_Error        .TypeMismatch]
    Default14m.noop(_.assertDataU(_.strings_('f)).using(_.toSeq.flatten.forall(_.contains("OO"))))
    Default14m.noop(_.assertDataU(_.strings_('f)).using(_.toSeq.flatten.forall(_.contains("oo"))))

    // ---------------------------------------------------------------------------
    Default01.noop(_.assertDataU(_.string('f), _.int('g)).using((f, g) => (f.size + g) < 10))
    Default01       .assertDataU(_.int   ('f), _.int('g)).using((f, g) => (f      + g) < 1).metaError[_Error        .TypeMismatch]
    Default01       .assertDataU(_.string('f), _.int('g)).using((f, g) => (f.size + g) < 1).dataError[_Error.Runtime.DataAssertionFailure]

    // ---------------------------------------------------------------------------
    Default51 .noop(_.assertDataU(_.string ('f)).using(_.contains("oo")))
  }

}

// ===========================================================================
