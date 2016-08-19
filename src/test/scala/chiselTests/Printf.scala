// See LICENSE for license details.

package chiselTests

import org.scalatest._
import chisel3._
import chisel3.testers.BasicTester

class SinglePrintfTester() extends BasicTester {
  val x = UInt(254)
  printf("x=%x", x)
  stop()
}

class ASCIIPrintfTester() extends BasicTester {
  printf((0x20 to 0x7e).map(_ toChar).mkString.replace("%", "%%"))
  stop()
}

class MultiPrintfTester() extends BasicTester {
  val x = UInt(254)
  val y = UInt(255)
  printf("x=%x y=%x", x, y)
  stop()
}

class CustomInterpolatorTester() extends BasicTester {
  val uint = Wire(init = UInt(123)

  val vec = Wire(Vec(4, UInt(width = 32)))
  for (i <- 0 until 4) { vec(i) := UInt(i) }

  val bundle = Wire(new Bundle {
    val foo = UInt(width = 32)
    val bar = UInt(width = 32)
  })
  bundle.foo := UInt(4)
  bundle.bar := UInt(5)

  val vecOfBundleOfVec = Wire(Vec(2, new Bundle {
    val foos = Vec(2, UInt(width = 32))
  }))
  vecOfBundleOfVec(0).foos(0) := UInt(6)
  vecOfBundleOfVec(0).foos(1) := UInt(7)
  vecOfBundleOfVec(1).foos(0) := UInt(8)
  vecOfBundleOfVec(1).foos(1) := UInt(9)

  val customPrintable = Wire(new Bundle {
    val foo = UInt(width = 32)
    val bar = UInt(width = 32)
    override def toPrintable: Printable =
      p"${Name(this)}\n" +
        Printables(for((name, value) <- elements) yield p"  $name: $value\n")
  })
  customPrintable.foo := UInt(10)
  customPrintable.bar := UInt(11)


  printf(p"${Name(uint)} = $uint = ${Decimal(uint)} = ${Binary(uint)}\n")
  printf(p"${Name(vec)} = $vec\n")
  printf(p"${Name(bundle)} = $bundle\n")
  printf(p"${Name(vecOfBundleOfVec)} = $vecOfBundleOfVec\n")
  printf(p"$customPrintable")
  printf(p"Printable" + " and String " + PString("concatination!\n"))

  stop()
}

class NegativePrintfTester() extends BasicTester {
  val (_, done) = Counter(Bool(true), 10)
  val x = Reg(init = SInt(-20))
  x := x + SInt(1)
  printf("%d\n", x)
  when (done) { stop() }
}


class PrintfSpec extends ChiselFlatSpec {
  "A printf with a single argument" should "run" in {
    assertTesterPasses { new SinglePrintfTester }
  }
  "A printf with multiple arguments" should "run" in {
    assertTesterPasses { new MultiPrintfTester }
  }
  "A printf with ASCII characters 1-127" should "run" in {
    assertTesterPasses { new ASCIIPrintfTester }
  }
  "A printf with Printable" should "run" in {
    assertTesterPasses { new CustomInterpolatorTester }
  }
  "A printf with negative arguments" should "run" in {
    assertTesterPasses { new NegativePrintfTester }
  }
}
