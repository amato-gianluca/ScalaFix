/**
 * Copyright 2015 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.fixpoint.structured

import it.unich.jandom.fixpoint._
import it.unich.jandom.fixpoint.finite._
import it.unich.jandom.utils.Relation
import it.unich.jandom.fixpoint.lattice.Magma

/**
 * This is the trait for a finite equation system which is composed of several layers. Each layer is
 * an equation system by itself, and may be though of as an hyperedges with many sources and a single
 * target.
 * @tparam U the type for the unknowns of this equation system.
 * @tparam V the type for the values assumed by the unknowns of this equation system.
 * @tparam E the type of hyperedges of this equation system
 */
abstract class GraphBasedEquationSystem[U, V: Magma, E] extends FiniteEquationSystem[U, V] {
  /**
   * It returns a possible initial value for the analysis
   */
  def initial: U => V

  /**
   * It returns a function which associates a transformer of assignments to each body.
   */
  def edgeBody: E => (U => V) => (U => V)

  /**
   * A relation between edges and unknowns. The pair (e,u) is in the relation if u is a source
   * of e. Note that edgeBody(e) may use the unknown u only if (e,u) is in the relation sources.
   */
  def sources: Relation[E, U]

  /**
   * A relation between edges and unknowns. The pair (e,u) is in the relation if u is a target
   * of e. Note that edgeBody(e)(rho)(x) is significant only if (e,x) is in the relation target.
   */
  def targets: Relation[E, U]

  /**
   * Add boxes to the equation system in a localized way.
   * @param boxes new box to add.
   * @param ordering an order on unknown in order to decide for which layer we need to apply widening.
   * @param boxesAreIdempotent if true the boxes are assumed to be idempotent and some optimization is possible.
   */
  def withLocalizedBoxes(boxes: BoxAssignment[U, V], ordering: Ordering[U], boxesAreIdempotent: Boolean): GraphBasedEquationSystem[U, V, _]

  /**
   * Combine a base assignment with the equation system
   * @param init the assignment to add to the equation system
   */
  def withBaseAssignment(init: PartialFunction[U, V]): FiniteEquationSystem[U, V]
}

object GraphBasedEquationSystem {
  import EquationSystem._

  /**
   * An alias for the type of the edgeBody method.
   */
  type EdgeBody[U, V, E] = E => (U => V) => (U => V)

  /**
   * Builds a layered equation system from a subset of its constituents.
   */
  def apply[U, V: Magma, E](unknowns: Iterable[U], inputUnknowns: Iterable[U], edgeBody: EdgeBody[U, V, E],
    sources: Relation[E, U], targets: Relation[E, U], initial: U => V) =
    SimpleGraphBasedEquationSystem(unknowns, inputUnknowns, edgeBody, sources, targets, initial)

  /**
   * An implementation of `LayeredEquationSystem` from a subset of its constituents.
   */
  final case class SimpleGraphBasedEquationSystem[U, V, E](
    val unknowns: Iterable[U],
    val inputUnknowns: Iterable[U],
    val edgeBody: EdgeBody[U, V, E],
    val sources: Relation[E, U],
    val targets: Relation[E, U],
    val initial: U => V)(implicit magma: Magma[V])
      extends GraphBasedEquationSystem[U, V, E] with FiniteEquationSystem.WithBaseAssignment[U, V] {

    private val ingoing = targets.inverse

    private val outgoing = sources.inverse

    val body = { (rho: U => V) =>
      (x: U) =>
        val contributions = for (e <- ingoing.image(x)) yield edgeBody(e)(rho)(x)
        // if contribution is empty the unknown x has no right hand side... it seems
        // reasonable to return the old value.
        if (contributions.isEmpty) rho(x) else contributions reduce magma.op
    }

    // TODO there is no way to force a particular ordering of the dependencies
    val withDependencies = { (rho: U => V) =>
      (x: U) => {
        val deps = for (e <- ingoing.image(x); y <- sources.image(e)) yield y
        val res = body(rho)(x)
        (res, deps)
      }
    }

    val infl = {
      val graph = for (x <- unknowns; e <- ingoing.image(x); y <- sources.image(e)) yield (y, x)
      val unknownsSet = unknowns.toSet
      Relation(graph.toSet, unknownsSet, unknownsSet)
    }

    def withBaseAssignment(init: PartialFunction[U, V]) = FiniteEquationSystem(
      body = addBaseAssignmentToBody(body, init),
      unknowns = unknowns,
      inputUnknowns = inputUnknowns,
      infl = infl)

    def withBoxes(boxes: BoxAssignment[U, V], boxesAreIdempotent: Boolean) = FiniteEquationSystem(
      body = addBoxesToBody(body, boxes),
      inputUnknowns = inputUnknowns,
      unknowns = unknowns,
      infl = if (boxesAreIdempotent) infl else infl union Relation(unknowns.toSet, { (u: U) => Set(u) }))

    def withLocalizedBoxes(boxes: BoxAssignment[U, V], ordering: Ordering[U], boxesAreIdempotent: Boolean) = {
      val newEdgeBody: EdgeBody[U, V, E] = { e: E =>
        (rho: U => V) => x: U =>
          if (boxes.isDefinedAt(x) && sources.image(e).exists { ordering.lteq(x, _) }) {
            boxes(x)(rho(x), edgeBody(e)(rho)(x))
          } else
            edgeBody(e)(rho)(x)
      }
      val newSources = if (boxesAreIdempotent)
        sources
      else {
        val newgraph: Set[(E, U)] = (for (x <- unknowns; e <- ingoing.image(x); if sources.image(e) exists { ordering.lteq(x, _) })
          yield (e, x))(collection.breakOut)
        sources union Relation(newgraph)
      }
      copy(edgeBody = newEdgeBody, sources = newSources)
    }
  }
}
