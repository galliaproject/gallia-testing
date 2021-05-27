package galliatest.suites.multiple

import gallia._

// ===========================================================================
object FilterTest extends gallia.testing.Suite {
import TestDataO._
import TestDataS._

  override def test() {
    Default52.filterBy(_.string('f))           .matches(_ == "foo2")               .force.one.check(Default01b)
  //Default52.filterBy(_.string('f))           .hasValue    ("foo2")               .force.one.check(Default01b)
    Default52.filterBy(         'f )           .matches(_ == "foo2")               .force.one.check(Default01b)
    Default52.filterBy(         'f )           .hasValue    ("foo2")               .force.one.check(Default01b)
    Default52.filterBy(_.string('f), _.int('g)).matches((f, g) => (f.size + g) > 4).force.one.check(Default01b)

    Default52.filterUnsafe { o => (o.string('f).size + o.int('g)) > 4 }            .force.one.check(Default01b)

    res0.filterBy(_.obj ('p)).matches(_.squash(_.string('f), _.int('g)).using(                (f, g) => (f.size + g) > 4  )).force.one.check(bobj('z -> false, 'p ->     Default01b ))
    res1.filterBy(_.objz('p)).matches(_.squash(_.string('f), _.int('g)).using(_.forall { case (f, g) => (f.size + g) > 4 })).force.one.check(bobj('z -> false, 'p -> Seq(Default01b)))

    // ---------------------------------------------------------------------------
  //Default52.filterBy(_.string('f)).hasValue("foo").check(bobjs(Default01, Default01))
    Default52.filterBy(         'f ).hasValue("foo").check(bobjs(Default01, Default01))
    Default52.findBy  (_.string('f)).hasValue("foo").check(bobjs(Default01))

    Default52.filterBy(_.string('f)).matches(_ == "foo").check(bobjs(Default01, Default01))
    Default52.findBy  (_.string('f)).matches(_ == "foo").check(bobjs(Default01))

    Default52.filterBy(_.string('f)).matches(_ == "FOO").checkEmpty()
    Default52.findBy  (_.string('f)).matches(_ == "FOO").checkEmpty()

    // ---------------------------------------------------------------------------
    Default57.filterBy(_.string_('f)).matches(_.isDefined).check(aobjs(Default13p, Default13p2, Default13p))
  //Default57.filterBy(_.any_('f))   .matches(_.isDefined) // TODO: allow?
    Default57.filterBy('f).isPresent                      .check(aobjs(Default13p, Default13p2, Default13p))
    Default57.filterBy('f).hasSize(1)                     .check(aobjs(Default13p, Default13p2, Default13p))
  }

}

// ===========================================================================
