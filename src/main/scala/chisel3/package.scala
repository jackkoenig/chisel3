package object chisel3 extends PrintableAPI {
  import scala.language.experimental.macros

  import internal.firrtl.Width
  import internal.sourceinfo.{SourceInfo, SourceInfoTransform}
  import util.BitPat


  type Direction = chisel3.core.Direction
  val INPUT = chisel3.core.INPUT
  val OUTPUT = chisel3.core.OUTPUT
  val NO_DIR = chisel3.core.NO_DIR
  type Flipped = chisel3.core.Flipped
  type Data = chisel3.core.Data
  val Wire = chisel3.core.Wire
  val Clock = chisel3.core.Clock
  type Clock = chisel3.core.Clock

  type Aggregate = chisel3.core.Aggregate
  val Vec = chisel3.core.Vec
  type Vec[T <: Data] = chisel3.core.Vec[T]
  type VecLike[T <: Data] = chisel3.core.VecLike[T]
  type Bundle = chisel3.core.Bundle

  val assert = chisel3.core.assert

  type Element = chisel3.core.Element
  type Bits = chisel3.core.Bits
  val Bits = chisel3.core.Bits
  type Num[T <: Data] = chisel3.core.Num[T]
  type UInt = chisel3.core.UInt
  val UInt = chisel3.core.UInt
  type SInt = chisel3.core.SInt
  val SInt = chisel3.core.SInt
  type Bool = chisel3.core.Bool
  val Bool = chisel3.core.Bool
  val Mux = chisel3.core.Mux

  type BlackBox = chisel3.core.BlackBox

  val Mem = chisel3.core.Mem
  type MemBase[T <: Data] = chisel3.core.MemBase[T]
  type Mem[T <: Data] = chisel3.core.Mem[T]
  val SeqMem = chisel3.core.SeqMem
  type SeqMem[T <: Data] = chisel3.core.SeqMem[T]

  val Module = chisel3.core.Module
  type Module = chisel3.core.Module

  val printf = chisel3.core.printf

  val Reg = chisel3.core.Reg

  val when = chisel3.core.when
  type WhenContext = chisel3.core.WhenContext


  implicit class fromBigIntToLiteral(val x: BigInt) extends AnyVal {
    def U: UInt = UInt(x, Width())
    def S: SInt = SInt(x, Width())
  }
  implicit class fromIntToLiteral(val x: Int) extends AnyVal {
    def U: UInt = UInt(BigInt(x), Width())
    def S: SInt = SInt(BigInt(x), Width())
  }
  implicit class fromStringToLiteral(val x: String) extends AnyVal {
    def U: UInt = UInt(x)
  }
  implicit class fromBooleanToLiteral(val x: Boolean) extends AnyVal {
    def B: Bool = Bool(x)
  }

  implicit class fromUIntToBitPatComparable(val x: UInt) extends AnyVal {
    final def === (that: BitPat): Bool = macro SourceInfoTransform.thatArg
    final def != (that: BitPat): Bool = macro SourceInfoTransform.thatArg
    final def =/= (that: BitPat): Bool = macro SourceInfoTransform.thatArg

    def do_=== (that: BitPat)(implicit sourceInfo: SourceInfo): Bool = that === x
    def do_!= (that: BitPat)(implicit sourceInfo: SourceInfo): Bool = that != x
    def do_=/= (that: BitPat)(implicit sourceInfo: SourceInfo): Bool = that =/= x
  }

 /** Implicit for custom String interpolators for BigInt literal creation */
  implicit class stringsToBigInt(val sc: StringContext) extends AnyVal {
    /** Generator for string interpolator functions */
    private def genInterpolator(
        args: Seq[Any],
        legalChars: Set[Char],
        ignoreChars: Set[Char],
        base: Int): BigInt = {
      require(sc.parts.size == 1 && args.size == 0,
        "Variables are not allowed!")
      require(sc.parts.head forall legalChars,
        "The only legal characters are " +
        (legalChars.toList.sorted map (c => s"'$c'") mkString ", "))
      BigInt(sc.parts.head filterNot ignoreChars, base)
    }
    // defs because fields aren't allowed in implicit class
    private def ignore = Set('_', ' ')
    private def binary = Set('0', '1')
    private def hex = ('a' to 'f').toSet ++ ('A' to 'F').toSet ++ ('0' to '9').toSet
    /* String interpolator for turning creating BigInts from binary literals
     * @example
     * {{{
     *  scala> val myBinNum = b"1010"
     *  res0: scala.math.BigInt = 10
     * }}}
     */
    def b(args: Any*): BigInt = genInterpolator(args, ignore ++ binary, ignore, 2)
    /* String interpolator for turning creating BigInts from hexadecimal literals
     * @example
     * {{{
     *  scala> val myBinNum = h"ff"
     *  res0: scala.math.BigInt = 255
     * }}}
     */
    def h(args: Any*): BigInt = genInterpolator(args, ignore ++ hex, ignore, 16)
  }

  /** Implicit for custom Printable string interpolator */
  implicit class PrintableHelper(val sc: StringContext) extends AnyVal {
    /** Custom string interpolator for generating Printables: p"..."
      * Will call .toString on any non-Printable arguments (mimicking s"...")
      */
    def p(args: Any*): Printable = {
      sc.checkLengths(args) // Enforce sc.parts.size == pargs.size + 1
      val pargs: Seq[Option[Printable]] = args map {
        case p: Printable => Some(p)
        case d: Data => Some(d.toPrintable)
        case any => for {
          v <- Option(any) // Handle null inputs
          str = v.toString
          if !str.isEmpty // Handle empty Strings
        } yield PString(str)
      }
      val parts = sc.parts map StringContext.treatEscapes
      // Zip sc.parts and pargs together ito flat Seq
      // eg. Seq(sc.parts(0), pargs(0), sc.parts(1), pargs(1), ...)
      val seq = for { // append None because sc.parts.size == pargs.size + 1
        (literal, arg) <- parts zip (pargs :+ None)
        optPable <- Seq(Some(PString(literal)), arg)
        pable <- optPable // Remove Option[_]
      } yield pable
      Printables(seq)
    }
  }

  implicit def string2Printable(str: String): Printable = PString(str)
}
