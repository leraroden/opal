package org.opalj.tac2bc

import org.opalj.tac.{V, Var}

import scala.collection.mutable

/** Gesammelte Info pro Var‑ID */
case class VarUsage(
    defSites:           mutable.Buffer[Int] = mutable.Buffer.empty[Int],
    useSites:           mutable.Buffer[Int] = mutable.Buffer.empty[Int],
    var varRef:         Option[Var[V]] = None,
    var storedInLocals: Boolean = false
)