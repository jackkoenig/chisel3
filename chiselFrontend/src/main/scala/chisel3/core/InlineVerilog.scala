
package chisel3.core

import scala.collection.mutable

import PrintableAPI._
import chisel3.internal._
import chisel3.internal.Builder.pushCommand
import chisel3.internal.firrtl
import chisel3.internal.firrtl.Arg
import chisel3.internal.sourceinfo.{SourceInfo, UnlocatableSourceInfo}

object InlineVerilog {
  case class UnsafeVerilogException(keyword: String) extends Exception(
    s"Verilog keyword $keyword is unsafe and can only be used if unsafe = true")

  case object IllegalUseOfDerefException extends Exception(
    "InlineVerilog.deref illegally used outside of inlineVerilog call")

  case class InvalidPrintableException(pable: Printable) extends Exception(
    s"InlineVerilog does not support Printable ${pable.getClass.getSimpleName}")

  // This should never happen
  case class CannotReserveIdentifierException(id: String) extends Exception(
    s"Could not reserve the identifier $id because it is already in the Namespace")

  // If found in inlineVerilog will throw error unless unsafe = true
  private val unsafeVerilogKeywords =
    Seq("assign", "module", "endmodule", "wire", "reg", "package")

  // Must match names injected by deref
  private val derefRegex = """(.*?)(\$\$CHISEL_DEREF(_\d+)?\$\$)""".r

  class VerilogContext private[InlineVerilog](currentModule: Module) {
    /** Dereference a circuit element
      *
      * @note must be called within [[inlineVerilog]]
      * @param ref Reference to [[Chisel.Element]]
      * @return a format String for the Chisel ref to replace
      */
    def deref(ref: Element): String = {
      val name = currentModule._namespace.name("$$CHISEL_DEREF") + "$$"
      currentModule._derefs += (name -> ref)
      name
    }

    /** Reserves a Verilog id
      *
      *  @note This MUST be called on all new ids, or this method will issue
      *    an error.
      *  @param id The identifier to reserve
      *  @returns The same identifier as id
      */
    def reserve(id: String): String =
      if (currentModule._namespace.name(id) == id) id
      else throw CannotReserveIdentifierException(id)

    /** Creates a reference to the current conditional predicate and inserts it
      *
      * @returns a format String for the Chisel ref to replace
      */
    def whenPredicate(implicit sourceInfo: SourceInfo): String = {
      // Declare a Wire, then move it's declaration to the front
      // This effectively creates the Wire outside of the when scope
      // This should be replaced with something like a when scope stack
      //   or walking the Chisel AST (once it isn't flat)
      val commands = currentModule._commands
      val beforeSize = commands.length
      val pred = Wire(init = false.asBool).suggestName("whenPredicate")
      val numCmds = commands.length - beforeSize
      val predCmds = commands.takeRight(numCmds)
      commands.trimEnd(numCmds) // Remove wire declaration
      commands.prependAll(predCmds) // Put wire declaration at beginning
      pred := true.asBool // pred wire will be true when predicate is true
      deref(pred)
    }
  }

  // Currently just checks for "unsafe" keywords
  private[this] def checkSafety(verilog: String): Unit =
    unsafeVerilogKeywords find (verilog contains _) foreach {
      keyword => throw UnsafeVerilogException(keyword)
    }

  // Finds all instances of the derefRegex in a String and replaces them
  // with [[VerilogName]] references to the corresponding Chisel Data
  private def handleDerefs(currentModule: Module, str: String): Printable = {
    val pables = mutable.ListBuffer.empty[Printable]
    for (line <- str split "\n") {
      val tail = derefRegex replaceAllIn (line, { m =>
        val elt = currentModule._derefs(m.group(2))
        currentModule._derefs -= m.group(2)
        pables append (PString(m.group(1)))
        pables append (VerilogName(elt))
        ""
      })
      pables append PString(tail)
      pables append ("\n")
    }
    Printables(pables dropRight 1) // remove trailing "\n"
  }

  private def inlineVerilogImpl(
      generator: VerilogContext => Printable,
      unsafe: Boolean): Unit = {
    // Get module so we can access its namespace
    val currentModule = Builder.forcedModule
    val pable: Printable = generator(new VerilogContext(currentModule))

    def process(pable: Printable): Printable = pable match {
      case PString(str) =>
        if (!unsafe) checkSafety(str)
        handleDerefs(currentModule, str)
      case Printables(xs) => Printables(xs map process)
      case _: FirrtlFormat =>
        throw new InvalidPrintableException(pable)
      case other => other
    }
    val res = process(pable)
    pushCommand(firrtl.InlineVerilog(UnlocatableSourceInfo, res))
  }

  /** Insert inline Verilog in the resulting output
    *
    * @param generator A function from [[VerilogContext]] to the [[Printable]]
    *   to inline. See example use in the documentation.
    * @note "unsafe" Verilog constructs like declaring elements and assignment
    *   are NOT allowed
    */
  def inlineVerilog(generator: VerilogContext => Printable): Unit =
    inlineVerilogImpl(generator, false)
  /** Insert unsafe inline Verilog in the resulting output
    *
    * @param generator A function from [[VerilogContext]] to the Verilog string
    *   to inline. See example use in the documentation.
    * @note "unsafe" Verilog constructs like declaring elements and assignment
    *   ARE allowed
    */
  def unsafeInlineVerilog(generator: VerilogContext => Printable): Unit =
    inlineVerilogImpl(generator, true)
}

private[chisel3] trait InlineVerilog {
  self: Module => // Only should be implemented by Module

  import InlineVerilog._

  // Defined in Module
  private[chisel3] val _namespace: Namespace

  // Outstanding dereferenced elements
  private[chisel3] val _derefs = mutable.HashMap[String, Element]()

  private[chisel3] def cleanupInlineVerilog(): Unit = {
    if (_derefs.nonEmpty) throw IllegalUseOfDerefException
  }
}
