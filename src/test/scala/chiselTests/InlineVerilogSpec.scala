package chiselTests

import org.scalatest.{FlatSpec, Matchers}
import scala.collection.mutable

import chisel3._
import chisel3.core.InlineVerilog._
import chisel3.testers.BasicTester

trait InlineVerilogTests {
  // Use BasicTester so we don't have to declare io
  class ExactVerilog extends BasicTester {
    inlineVerilog { ctx =>
      import ctx._
      """always @(posedge clock)"""
    }
  }
  class SimpleDeref extends BasicTester {
    inlineVerilog { ctx =>
      import ctx._
      s"""always @(posedge ${deref(this.clock)} || ${deref(this.reset)})"""
    }
  }
  class CompoundDeref extends BasicTester {
    val a = Wire(UInt(width = 4))
    val b = Wire(UInt(width = 4))
    inlineVerilog { ctx =>
      import ctx._
      s"""always @(${deref(a + b)})"""
    }
  }
  class BundleDeref extends BasicTester {
    val foo = Wire(new Bundle { val bar = UInt(width = 4) })
    inlineVerilog { ctx =>
      import ctx._
      s"""always @(${deref(foo.bar)})"""
    }
  }
  class DisallowUnsafe extends BasicTester {
    inlineVerilog { ctx =>
      import ctx._
      """assign reset <= clk"""
    }
  }
  class DisallowIllegalUseOfDeref extends BasicTester {
    inlineVerilog { ctx =>
      import ctx._
      deref(this.clock)
      "deref outside of resulting string!!!"
    }
  }
  class DisallowMultiReserveOfSameId extends BasicTester {
    inlineVerilog { ctx =>
      import ctx._
      s"""|wire [3:0] ${reserve("state")};
          |wire [3:0] ${reserve("state")};""".stripMargin
    }

  }
  def genCoverage(clk: Clock, elt: Element, range: Range): Unit =
    for (i <- range) {
      inlineVerilog { ctx =>
        import ctx._
        s"""cover property (@(posedge ${deref(clk)}) ${deref(elt)} == $i);"""
      }
    }
  class InlineVerilogOutsideModule extends BasicTester {
    val (count, _) = Counter(true.asBool, 4)
    genCoverage(this.clock, count, 0 until 4)
  }
  class AllowIdentifierReservation extends BasicTester {
    val state = Reg(init = (0, width = 4))
    state := state + UInt(1)

    // Split lines so they end up in separate vinlines
    unsafeInlineVerilog { ctx =>
      import ctx._
      s"""wire [3:0] ${reserve("state")};"""
    }
    unsafeInlineVerilog { ctx =>
      import ctx._
      s"""assert(state == ${deref(state)});"""
    }
  }
  class AllowModuloOperator extends BasicTester {
    val (count, _) = Counter(true.asBool, 4)

    // Split lines so they end up in separate vinlines
    unsafeInlineVerilog { ctx =>
      import ctx._
      s"""wire ${reserve("foo")};"""
    }
    unsafeInlineVerilog { ctx =>
      import ctx._
      s"""assign foo = ${deref(count)} % 2'd2;"""
    }
  }
}

/* Inline Verilog Tests
 *
 * @note These tests make assumptions about the names coming from Chisel
 *   Fortunately, these names should be stable, but using some API for asking
 *   Chisel for the names would be more robust
 */
class InlineVerilogSpec extends FlatSpec with Matchers with InlineVerilogTests {
  private val InlineRegex = """\s*vinline\("(.*)"((?:[\s,]*\S+\s*)*)\)\s*""".r
  private case class Inline(str: String, args: Seq[String])
  private def getInlines(firrtl: String): Seq[Inline] = {
    def getArgs(args: String): Seq[String] =
      args split (",") map (_.trim) filter (_.nonEmpty)

    firrtl split ("\n") flatMap {
      case InlineRegex(str, args) => Some(Inline(str, getArgs(args)))
      case _ => None
    }
  }

  behavior of "Inline Verilog"

  it should "pass exact Verilog through" in {
    val firrtl = Driver.emit(() => new ExactVerilog)
    getInlines(firrtl) match {
      case Seq(Inline("""always @(posedge clock)""", Seq())) =>
      case _ => fail()
    }
  }
  it should "allow dereferencing" in {
    val firrtl = Driver.emit(() => new SimpleDeref)
    getInlines(firrtl) match {
      case Seq(Inline("""always @(posedge %I || %I)""", Seq("clk", "reset"))) =>
      case _ => fail()
    }
  }
  it should "allow dereferncing Chisel statements" in {
    val firrtl = Driver.emit(() => new CompoundDeref)
    getInlines(firrtl) match {
      case Seq(Inline("""always @(%I)""", Seq(temporary))) =>
        assert(temporary != "a")
        assert(temporary != "b")
      case _ => fail()
    }
  }
  it should "dereference bundle fields" in {
    val firrtl = Driver.emit(() => new BundleDeref)
    getInlines(firrtl) match {
      case Seq(Inline("""always @(%I)""", Seq("foo.bar"))) =>
      case _ => fail()
    }
  }
  it should "disallow unsafe by default" in {
    intercept[UnsafeVerilogException] {
      Driver.emit(() => new DisallowUnsafe)
    }
  }
  it should "disallow calling deref outside of an inlineVerilog call" in {
    // Must catch then match because intercept doesn't work on case objects
    intercept[Exception] {
      Driver.emit(() => new DisallowIllegalUseOfDeref)
    } match {
      case IllegalUseOfDerefException => // success
      case _ => fail("Wrong exception thrown")
    }
  }
  it should "disallow reserving the same identifier multiple times" in {
    intercept[CannotReserveIdentifierException] {
      Driver.emit(() => new DisallowMultiReserveOfSameId)
    }
  }
  it should "allow inlineVerilog to be called outside of a Chisel.Module" in {
    val firrtl = Driver.emit(() => new InlineVerilogOutsideModule)
    getInlines(firrtl) match {
      case Seq(Inline("cover property (@(posedge %I) %I == 0);", Seq("clk", "count")),
               Inline("cover property (@(posedge %I) %I == 1);", Seq("clk", "count")),
               Inline("cover property (@(posedge %I) %I == 2);", Seq("clk", "count")),
               Inline("cover property (@(posedge %I) %I == 3);", Seq("clk", "count"))) =>
      case _ => fail()
    }
  }
  it should "Allow identifiers to be reserved" in {
    val firrtl = Driver.emit(() => new AllowIdentifierReservation)
    getInlines(firrtl) match {
      case Seq(Inline("""wire [3:0] state;""", Seq()),
               Inline("""assert(state == %I);""", Seq(state))) =>
        assert(state != "state") // "state" was reserved
      case _ => fail()
    }
  }
  it should "Allow modulo division" in {
    val firrtl = Driver.emit(() => new AllowModuloOperator)
    getInlines(firrtl) match {
      case Seq(Inline("""wire foo;""", Seq()),
               Inline("""assign foo = %I %% 2'd2;""", Seq("count"))) =>
      case _ => fail()
    }
  }
}
