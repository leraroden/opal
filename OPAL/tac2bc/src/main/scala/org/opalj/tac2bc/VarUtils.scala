package org.opalj.tac2bc

import org.opalj.tac.{DVar, UVar, V, Var}
import org.opalj.value.ValueInformation

object VarUtils {

    /** Liefert die TAC-Definition-ID jeder Var (egal ob DVar oder UVar). */
    def getVarId(variable: Var[V], tacToLVIndex: Map[Int, Int]): Int = {
        val tacIndex = variable match {
            case dVar: DVar[ValueInformation] => dVar.originatedAt
            case uVar: UVar[ValueInformation] => uVar.definedBy.head
        }
        tacToLVIndex.getOrElse(tacIndex, throw new RuntimeException(s"no id found for variable $variable"))
    }
}
