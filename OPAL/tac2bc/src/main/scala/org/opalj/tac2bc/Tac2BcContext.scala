package org.opalj.tac2bc

import org.opalj.ba.CodeElement
import org.opalj.br.analyses.SomeProject
import org.opalj.br.instructions.{DUP, DUP2, RewriteLabel}
import org.opalj.collection.immutable.IntTrieSet
import org.opalj.tac.{ArrayLoad, Assignment, BinaryExpr, Call, Const, DVar, Expr, GetStatic, If, New, NewArray, Stmt, UVar, V, Var}
import org.opalj.tac2bc.ExprProcessor.{processArrayLoad, processBinaryExpr, processCall}
import org.opalj.value.ValueInformation

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * Context for translating TAC to bytecode in reverse order.
 */
class Tac2BcContext(
    tacStmts     : Array[(Stmt[V], Int)],
    tacToLVIndex : Map[Int,Int],
    code         : ListBuffer[CodeElement[Nothing]],
    labels       : Array[RewriteLabel]
)(implicit project: SomeProject) {

    /** Remaining uses of a variable after its definition. */
    private val usesLeft = mutable.Map[Int, Int]()

    /** Maps each variable to its definition index in TAC. */
    private val savedDefSites = mutable.Map[Var[V], IntTrieSet]()

    val visitedStmt: ArrayBuffer[Stmt[V]] = ArrayBuffer[Stmt[V]]()

    val delayedVisitStmt: ArrayBuffer[Stmt[V]] = ArrayBuffer[Stmt[V]]()

    val loadedStmt: ArrayBuffer[Var[V]] = ArrayBuffer[Var[V]]()

    def emitStmt(defIdx: Int,
                 delayStmtVisit: Boolean = false,
                 nestedStmt: Boolean = false,
                 parentIdx: Int = -1): Unit = {
        val variable = getVarFromId(defIdx)
        val stmt = tacStmts(defIdx)._1
        val stmtIndex = tacStmts(defIdx)._2
        if (!savedDefSites.contains(variable)) saveVariableInfo(variable)

        // Determine where this variable is used
        val usedIdx = variable match {
            case dvar: DVar[ValueInformation] => dvar.usedBy.head
            case uvar: UVar[ValueInformation] => uvar.definedBy.head
        }

        val childIdx = variable match {
            case dvar: DVar[ValueInformation] => dvar.usedBy.iterator.min
            case uvar: UVar[ValueInformation] => uvar.definedBy.toList.iterator.min
        }

        if (childIdx == parentIdx && loadedStmt.contains(variable)) {
            StmtProcessor.processStmt(stmt, tacToLVIndex, labels, code, this, stmtIndex, delayStmtVisit, nestedStmt, endNode = true)
        }
        else if (getDefSize(usedIdx, defIdx) > 1 || getUseSites(defIdx) > 1) {
            emitVarDef(variable)
        } else {
            StmtProcessor.processStmt(stmt, tacToLVIndex, labels, code, this, stmtIndex, delayStmtVisit, nestedStmt)
        }
    }

    def emitVarUse(variable: Var[V],
                   delayStmtVisit: Boolean = false,
                   nestedStmt: Boolean): Unit = {
        if (!savedDefSites.contains(variable)) saveVariableInfo(variable)
        val defSites = getIndicesFromVariable(variable)
        val defIdx = defSites.head

        // Determine the First use-site index (where this variable is used)
        val usedIdx = variable match {
            case dvar: DVar[ValueInformation] => dvar.usedBy.head
            case uvar: UVar[ValueInformation] => uvar.definedBy.head
        }

        // If the current expression has multiple def-sites,
        // store the variable in a local to preserve its value.
//        if(getDefSize(usedIdx, defIdx) > 1) {
//            emitMultDef(variable, defSites, delayStmtVisit, nestedStmt)
//        } else if (getUseSites(defIdx) > 1) {
//            emitMultUse(variable, defIdx, delayStmtVisit, nestedStmt)

        if (getDefSize(usedIdx, defIdx) > 1 || getUseSites(defIdx) > 1) {
            emitMultUse(variable, defIdx, delayStmtVisit, nestedStmt)
        } else {
            emitDef(defIdx, delayStmtVisit, nestedStmt)
        }
    }

    /**
     * Handles variables with multiple definition sites.
     */
    def emitVarDef(variable: Var[V]): Unit = {
        if (!savedDefSites.contains(variable)) saveVariableInfo(variable)
        ExprProcessor.loadVariable(variable, tacToLVIndex, code)
        loadedStmt += variable
    }

    /**
     * Maintains essential tracking information for a variable.
     * Registers its definition sites in `savedDefSites` and initializes remaining use counts in `usesLeft`.
     */
    private def saveVariableInfo(variable: Var[V]): Unit = {
        val defSites = getIndicesFromVariable(variable)
        savedDefSites.getOrElseUpdate(variable, defSites)

        defSites.iterator.foreach(defIdx => usesLeft.getOrElseUpdate(defIdx, getUseSites(defIdx)))
    }

    /**
     * Emits store of variables in locals with multiple uses.
     */
    private def emitMultUse(variable: Var[V],
                            defIdx: Int,
                            delayStmtVisit: Boolean,
                            nestedStmt: Boolean): Unit = {
        ExprProcessor.storeVariable(variable, tacToLVIndex, code)
        if (variable.cTpe.isCategory2) code += DUP2 else code += DUP
        emitDef(defIdx, delayStmtVisit, nestedStmt)
    }

//    /**
//     * Emits store of variables in locals with multiple definition sites.
//     */
//    private def emitMultDef(variable: Var[V],
//                            defSites: IntTrieSet,
//                            delayStmtVisit: Boolean,
//                            nestedStmt: Boolean): Unit = {
//        ExprProcessor.storeVariable(variable, tacToLVIndex, code)
//
//        defSites.iterator.foreach { defIdx =>
//            emitDef(defIdx, delayStmtVisit, nestedStmt)
//        }
//    }

    /**
     * Emits bytecode for the definition of a variable at the given index.
     * Loads a constant onto the stack.
     */
    private def emitDef(defIdx: Int, delayStmtVisit: Boolean, nestedStmt: Boolean): Unit = {
        val stmt = tacStmts(defIdx)._1
        val stmtIndex = tacStmts(defIdx)._2
        stmt match {
            case Assignment(_, _, expr) =>
                expr match {
                    case const: Const => ExprProcessor.loadConstant(const, code)
                    case newArray: NewArray[V] => ExprProcessor.processNewArray(newArray, tacToLVIndex, code, this)
                    case newExpr: New => ExprProcessor.processNewExpr(newExpr.tpe, code)
                    case getStatic: GetStatic => ExprProcessor.processGetStatic(getStatic, code)
                    case callExpr: Call[V @unchecked] =>
                        val call @ Call(declaringClass, isInterface, name, descriptor) = callExpr
                        processCall(
                            call,
                            declaringClass,
                            isInterface,
                            name,
                            descriptor,
                            tacToLVIndex,
                            code,
                            this,
                            stmtIndex
                        )
                    case arrayLoadExpr: ArrayLoad[V] => processArrayLoad(arrayLoadExpr, tacToLVIndex, code, this, delayStmtVisit, stmtIndex)
                    case binaryExpr: BinaryExpr[V] => processBinaryExpr(binaryExpr, tacToLVIndex, code, this, nestedStmt, stmtIndex)
                    case _ =>
                }
            case _ =>
        }
    }

    /**
     * Returns all definition indices associated with the given variable.
     */
    def getIndicesFromVariable(variable: Var[V]): IntTrieSet = {
        savedDefSites.getOrElseUpdate(
            variable, {
                variable match {
                    case dvar: DVar[ValueInformation] =>
                        IntTrieSet(dvar.originatedAt)

                    case uvar: UVar[ValueInformation] => uvar.definedBy
                    case _ =>
                        IntTrieSet.empty
                }
            }
        )
    }

    /**
     * Returns the number of use-sites for a variable at the given definition index.
     */
    private def getUseSites(defIdx: Int): Int = {
        tacStmts(defIdx)._1 match {
            case Assignment(_, dvar: DVar[ValueInformation], _) => dvar.usedBy.size
            case _ => throw new NoSuchElementException("There are no variables in Statements.")
        }
    }

    /**
     * Returns the number of def-sites for the variable used inside the expression
     * of the statement at the given definition index.
     */
    private def getDefSize(useIdx: Int, defIdx: Int): Int = {
        tacStmts(useIdx)._1 match {
            case Assignment(_, _, expr) =>
                //muss für ein DVar auch gemacht werden
                findUVarInExpr(expr, defIdx)
                    .map(_.asVar.definedBy.size)
                    .getOrElse(0)
            case If(_, leftExpr, _, rightExpr, _) =>
                if(leftExpr.asVar.definedBy.contains(defIdx))
                    findUVarInExpr(leftExpr, defIdx)
                        .map(_.asVar.definedBy.size)
                        .getOrElse(0)
                else
                    findUVarInExpr(rightExpr, defIdx)
                        .map(_.asVar.definedBy.size)
                        .getOrElse(0)
            case _ => 0
        }
    }

    /**
     * Returns the variable corresponding to the given definition index.
     */
    private def getVarFromId(defIdx: Int): Var[V] = {
        tacStmts(defIdx)._1 match {
            case Assignment(_, dvar: DVar[ValueInformation], _) => dvar.asVar
            case _ => throw new NoSuchElementException("There are no variables in Statements.")
        }
    }

    /** Checks if a TAC statement has already been visited. */
    def isStmtVisited(stmt: Stmt[V]): Boolean = {
        visitedStmt.contains(stmt)
    }

    /** Checks if a TAC statement has been delayed. */
    def isStmtVisitDelayed(stmt: Stmt[V]): Boolean = {
        delayedVisitStmt.contains(stmt)
    }

    /**
     * Returns the given expression and returns the first UVar found, if any.
     */
    private def findUVarInExpr(expr: Expr[V], defIdx: Int): Option[UVar[_]] = {
        // First, check the root expression itself.
        var found: Option[UVar[_]] = expr match {
            case u: UVar[_] if u.definedBy.contains(defIdx) => return Some(u)
            case _ => None
        }

        // Run through all subexpressions.
        // It will stop traversal when the predicate returns false.
        expr.forallSubExpressions { sub =>
            sub match {
                case u: UVar[_] if u.definedBy.contains(defIdx) =>
                    found = Some(u)
                    false
                case _ =>
                    true
            }
        }
        found
    }

    def isStmtLoaded(stmtIndex: Int): Boolean = {
        loadedStmt.exists {
            case dvar: DVar[ValueInformation] => dvar.originatedAt == stmtIndex
            case uvar: UVar[ValueInformation] => uvar.definedBy.head == stmtIndex
            case _ => false
        }
    }
}
