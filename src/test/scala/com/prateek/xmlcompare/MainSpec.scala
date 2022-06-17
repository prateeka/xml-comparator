package com.prateek.xmlcompare
import org.scalatest.funspec.AnyFunSpec

class MainSpec extends AnyFunSpec {
  describe("Comparator behavior") {
    describe(
      "when one file is indented and corresponding match unindented"
    ) {
      it("should return matching request file names") {
        val f1 = getClass.getResource("/indented/1-req.xml").getFile
        val s1 = getClass.getResource("/nonindented/1-req.xml").getFile
        val args = s"-f $f1 -s $s1".split("\\s+")
        /*
        val crs = Main.execute(args)
        assert(crs.sizeIs == 1)
        crs.foreach({
          case Subset(first, second)
              if first.getAbsolutePath.equals(f1) && second.getAbsolutePath
                .equals(s1) =>
            succeed
          case _ => fail("expected match but found none")
        })
         */
      }

      /*      it("should return no match when matching request file not found") {
        val f1 = getClass.getResource("/indented/2-req.xml").getFile
        val s1 = getClass.getResource("/nonindented/1-req.xml").getFile
        val args = s"-f $f1 -s $s1".split("\\s+")
        val crs = Main.execute(args)
        assert(crs.sizeIs == 1)
        crs.foreach({
          case FileNotFound(f, s, NodeNotFound(n, cc))
              if f.getAbsolutePath.equals(f1) &&
                s.getAbsolutePath.equals(s1) &&
                n.equalsIgnoreCase(
                  "Discover.Restrictions.RestrictionList.PropertyName"
                ) &&
                cc.equalsIgnoreCase("ChildCount$") =>
            succeed
          case Subset(f, s) =>
            fail(s"expected no match but found $f matching $s")
        })
      }*/
    }
  }
}
