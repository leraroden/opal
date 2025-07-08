package org.opalj.tac2bc

import org.opalj.br.{Category1ComputationalTypeCategory, Category2ComputationalTypeCategory, ComputationalTypeCategory}
import org.opalj.br.instructions.InstructionLike
import org.opalj.control.repeat
import org.opalj.tac.{V, Var}
import org.opalj.tac2bc.VarUtils.getVarId

import scala.collection.mutable

/** Wo befindet sich ein TAC-Wert gerade? */
sealed trait Location
case class OnStack(depthFromTop: Int) extends Location
case class InLocal(index: Int)        extends Location
case object NotAvailable              extends Location

/**
 * Simuliert den JVM-Operand-Stack und die Lokals während der TAC->BC-Übersetzung.
 *
 * @param stack  Die Liste der aktuell auf dem Stack liegenden TAC-Werte (mit ihren CTGs).
 * @param varLocations Für jede TAC-Variable, wo sie sich gerade befindet.
 */
case class FrameState(
    var stack: mutable.Stack[(V, ComputationalTypeCategory)],
    var varLocations: mutable.Map[Var[V], Location]
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

        //varLocations anpassen
        if (v1 != null) varLocations(v1) = OnStack(1)
        if (v2 != null) varLocations(v2) = OnStack(0)
    }

    /** Simuliert den Stack und ändert den aktuellen FrameState. */
    def updateFrame(instr: InstructionLike): Unit = {
        val pops = instr.numberOfPoppedOperands(x => stack(x)._2)
        popFromStack(pops)

        val pushes = instr.numberOfPushedOperands(x => stack(x)._2)
        pushToStack(pushes, instr)
    }

    //TODO operandsize testen
    private def popFromStack(pops: Int): Unit = {
        var remainingSlots = pops
        while (remainingSlots > 0) {
            val (v, ctg) = stack.pop()           // poppt *einen* Wert (category-1 oder -2)
            remainingSlots -= ctg.operandSize    // zieht 1 oder 2 Slots ab
            if (v != null) {
                varLocations(v) = NotAvailable
            }
            decrementAllStackDepths(ctg.operandSize)
        }
    }

    /** Pusht `n` leere Slots als Platzhalter.*/
    private def pushToStack(n: Int, instr: InstructionLike): Unit = {
        repeat(n) {
            incrementAllStackDepths()
            val ctg = if (instr.stackSlotsChange == 2)
                Category2ComputationalTypeCategory
            else
                Category1ComputationalTypeCategory
            stack.push((null.asInstanceOf[V], ctg))
        }
    }

    /** Nach einem Push: erhöhe alle Tiefen um 1,
     *  damit OnStack(0) wieder frei ist für das neue Element. */
    private def incrementAllStackDepths(): Unit = {
        varLocations.mapValuesInPlace { case (v, location) =>
            location match {
                case OnStack(d) => OnStack(d + 1)
                case other      => other
            }
        }
    }

    /** Nach einem Pop:
     *  reduziere alle Tiefen um s (Anpassung je nach operandSize!). */
    def decrementAllStackDepths(s: Int): Unit = {
        varLocations.mapValuesInPlace { case (v, location) =>
            location match {
                case OnStack(d) =>
                    val newDepth = d - s
                    if (newDepth >= 0) OnStack(newDepth) else NotAvailable
                case other      => other
            }
        }
    }

    /** Sucht den Location-Eintrag für `v`, indem man über die Keys matcht. */
    def locationOf(v: V, tacToLVIndex: Map[Int, Int]) : Option[Location] = {
        val id = getVarId(v, tacToLVIndex)
        varLocations.collectFirst {
            case (key, location) if getVarId(key, tacToLVIndex) == id => location
        }
    }
}
