package org.opalj.tac2bc

import org.opalj.br.{Category1ComputationalTypeCategory, Category2ComputationalTypeCategory, ComputationalTypeCategory, ComputationalTypeFloat, ComputationalTypeInt}
import org.opalj.br.instructions.{AddInstruction, InstructionLike, NumericConversionInstruction}
import org.opalj.control.repeat
import org.opalj.tac.{V, Var}
import org.opalj.tac2bc.VarUtils.getVarId

import scala.collection.mutable

/**
 * Simuliert den JVM-Operand-Stack und die Lokals während der TAC->BC-Übersetzung.
 *
 * @param stack  Die Liste der aktuell auf dem Stack liegenden TAC-Werte (mit ihren CTGs).
 * @param localVarSlots Die Liste der aktuell in Locals liegenden TAC-Werte.
 */
case class FrameState(
    var stack: mutable.Stack[(V, ComputationalTypeCategory)],
    var localVarSlots: mutable.Map[Var[V], Int]
    ) {

    /** Dupliziert das oberste Element. */
    def dup(): Unit = {
        stack.push(stack.top)
    }

    /** Vertauscht die obersten beiden Elemente. */
    def swap(): Unit = {
        val (v1, ctg1) = stack.pop()
        val (v2, ctg2) = stack.pop()

        stack.push((v1, ctg1))
        stack.push((v2, ctg2))
    }

    def loadLocal(v: Var[V]): Unit = {
        localVarSlots.remove(v)
        val variable: V = v.asVar
        val ctg = getCTG(variable)
        stack.push((variable, ctg))
    }

    def storeLocal(v: Var[V]): Unit = {
        val usedSlots = localVarSlots.values
        val nextIdx = if (usedSlots.isEmpty) 0 else usedSlots.max + 1
        localVarSlots(v) = nextIdx
    }

    private def getCTG(v: V): ComputationalTypeCategory = {
        val ctg = v.cTpe match {
            case ComputationalTypeInt         => Category1ComputationalTypeCategory
            case ComputationalTypeFloat       => Category1ComputationalTypeCategory
        }
      ctg
    }

    /** Simuliert den Stack und ändert den aktuellen FrameState. */
    def updateFrame(instr: InstructionLike): Unit = {
        val pops = instr.numberOfPoppedOperands(x => stack(x)._2)
        popFromStack(pops)

        val pushes = instr.numberOfPushedOperands(x => stack(x)._2)
        pushToStack(pushes, instr)
    }

    //TODO operandsize testen
    private def popFromStack(n: Int): Unit = {
        repeat(n) {
            val (v, _) = stack.pop()
//            if (v != null) {
//                varLocations(v) = NotAvailable
//            }
            //decrementAllStackDepths()
        }
    }

    /** Pusht `n` leere Slots als Platzhalter.*/
    private def pushToStack(n: Int, instr: InstructionLike): Unit = {
        val ctg: ComputationalTypeCategory = instr match {
            case conv: NumericConversionInstruction =>
                // z.B. l2d -> Double, d2l -> Long
                if (conv.targetType.computationalType.operandSize == 2)
                    Category2ComputationalTypeCategory
                else
                    Category1ComputationalTypeCategory
            case op: AddInstruction =>
                //z.B. IADD, DADD
                if (op.computationalType.isCategory2)
                    Category2ComputationalTypeCategory
                else
                    Category1ComputationalTypeCategory
            case _ =>
                if (instr.stackSlotsChange > 1) Category2ComputationalTypeCategory
                else Category1ComputationalTypeCategory
        }

        repeat(n) {
            //incrementAllStackDepths()
            stack.push((null.asInstanceOf[V], ctg))
        }
    }

    def getStackIndex(variable: V): Int = {
        stackIndexOf(variable).getOrElse(
            throw new NoSuchElementException(s"Variable $variable not on stack")
        )
    }

    private def stackIndexOf(variable: V): Option[Int] = {
        stack.iterator
            .zipWithIndex
            .collectFirst { case ((v, _), depth) if v == variable => depth }
    }

    private def localIndexOf(variable: Var[V]): Option[Int] = {
        localVarSlots.get(variable)
    }

    def getLocalIndex(variable: Var[V]): Int = {
        localIndexOf(variable).getOrElse(
            throw new NoSuchElementException(s"Variable $variable not in locals")
        )
    }

    def isOnStack(variable: V): Boolean = {
        stack.exists { case (v, _) => v == variable }
    }

    def isInLocals(variable: Var[V]): Boolean = {
        localVarSlots.contains(variable)
    }
}
