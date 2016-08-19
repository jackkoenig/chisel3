package chiselTests

import chisel3._
import chisel3.InlineVerilog._
import chisel3.testers.BasicTester

// Separate file from InlineVerilogSpec to separate JUnit dependency
class InlineVerilog extends Module {
  // Assign module port types
  val io = IO(new Bundle {
    val en = Input(Bool())
    val in = Input(UInt(width = 32))
    val out = Output(UInt(width = 32)) // no connections except in inlined Verilog
    val odd = Output(Bool()) // no connections except in inlined Verilog
  })
  val myReg = Reg(init = UInt(h"deadbeef", width = 32))
  val myWire = Wire(UInt(width = 32)) // no connections except in inlined Verilog
  when (io.en) {
    myReg := io.in
  } .otherwise {
    myReg := myWire
  }
  when (myReg(0)) {
    unsafeInlineVerilog { ctx =>
      import ctx._
      s"""assign ${deref(myWire)} = ${deref(myReg)} + 32'd1;
         |assign ${deref(io.out)} = ${deref(myReg)};
         |assign ${deref(io.odd)} = $whenPredicate;""".stripMargin
    }
  }
}
class InlineVerilogTester extends BasicTester {
  val SetValue = 10
  val dut = Module(new InlineVerilog)
  val (count, done) = Counter(true.asBool, 10)

  dut.io.in := UInt(SetValue)
  dut.io.en := false.asBool
  when (count === UInt(0)) {
    dut.io.en := true.asBool
    assert(dut.io.out === UInt(h"deadbeef", width = 32))
  } .otherwise {
    assert(dut.io.out === UInt(SetValue - 1) + count)
  }
  assert(dut.io.out(0) === dut.io.odd)

  when (done) { stop() }
}

