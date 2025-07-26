package org.opalj.tac2bc

import org.opalj.br.{Category1ComputationalTypeCategory, Category2ComputationalTypeCategory, ComputationalTypeCategory, ComputationalTypeDouble, ComputationalTypeFloat, ComputationalTypeInt, ComputationalTypeLong, ComputationalTypeReference, ComputationalTypeReturnAddress}
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

    def dup_x2(): Unit = {
        require(stack.size >= 3, "Stack has too few elements for DUP_X2")

        val (v1, ctg1) = stack.pop()
        val (v2, ctg2) = stack.pop()

        if (v2.cTpe.isCategory2) {
            //TODO
        } else {
            val (v3, cat3) = stack.pop()
            // Form: value3, value2, value1 (alle Category 1)
            stack.push((v1, ctg1))
            stack.push((v3, cat3))
            stack.push((v2, ctg2))
            stack.push((v1, ctg1))
        }
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
            case ComputationalTypeInt           => Category1ComputationalTypeCategory
            case ComputationalTypeFloat         => Category1ComputationalTypeCategory
            case ComputationalTypeDouble        => Category2ComputationalTypeCategory
            case ComputationalTypeLong          => Category2ComputationalTypeCategory
            case ComputationalTypeReference     => Category1ComputationalTypeCategory
            case ComputationalTypeReturnAddress => Category1ComputationalTypeCategory
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
            stack.pop()
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
            stack.push((null.asInstanceOf[V], ctg))
        }
    }

    /**
     * Liefert den Stack‑Index einer Variable per TAC‑ID.
     */
    def getStackIndex(variable: V, tacToLVIndex: Map[Int, Int]): Int = {
        stackIndexOf(variable, tacToLVIndex).getOrElse {
            val id = getVarId(variable, tacToLVIndex)
            throw new NoSuchElementException(
                s"Variable $variable (TAC‑ID=$id) not on stack"
            )
        }
    }

    /**
     * Versucht, per TAC‑ID die Tiefe der Variable zu finden.
     */
    private def stackIndexOf(
        variable: Var[V],
        tacToLVIndex: Map[Int, Int]
    ): Option[Int] = {
        val targetId = getVarId(variable, tacToLVIndex)
        stack.iterator.zipWithIndex.collectFirst {
            case ((v, _), depth)
                if v != null &&
                    getVarId(v.asInstanceOf[Var[V]], tacToLVIndex) == targetId =>
                depth
        }
    }

    /**
     * Versucht, per TAC‑ID den Local‑Slot der Variable zu finden.
     */
    private def localIndexOf(
        variable: Var[V],
        tacToLVIndex: Map[Int, Int]
    ): Option[Int] = {
        val targetId = getVarId(variable, tacToLVIndex)
        localVarSlots.collectFirst {
            case (key, idx) if getVarId(key, tacToLVIndex) == targetId => idx
        }
    }

    /**Liefert den Local‑Slot‑Index per TAC‑ID */
    def getLocalIndex(
        variable: Var[V],
        tacToLVIndex: Map[Int, Int]
    ): Int = {
        localIndexOf(variable, tacToLVIndex).getOrElse {
            val id = getVarId(variable, tacToLVIndex)
            throw new NoSuchElementException(
                s"Variable $variable (TAC‑ID=$id) not in locals"
            )
        }
    }

    /**
     * Checks whether the given TAC variable is currently stored in the operand stack.
     */
    def isOnStack(variable: V, tacToLVIndex: Map[Int, Int]): Boolean = {
        val targetId = getVarId(variable, tacToLVIndex)
        stack.iterator.exists { case (v, _) =>
            v != null && getVarId(v.asInstanceOf[Var[V]], tacToLVIndex) == targetId
        }
    }

    /**
     * Checks whether the given TAC variable is currently stored in the local‐variable map.
     */
    def isInLocals(variable: Var[V], tacToLVIndex: Map[Int, Int]): Boolean = {
        val targetId = getVarId(variable, tacToLVIndex)
        localVarSlots.exists{ case (v, _) =>
            v != null && getVarId(v, tacToLVIndex) == targetId
        }
    }
}
