/**
  * Copyright 2015, 2016, 2017 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of ScalaFix.
  * ScalaFix is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * ScalaFix is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix. If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix.finite

import it.unich.scalafix._
import it.unich.scalafix.lattice.{Domain, Magma}
import it.unich.scalafix.utils.Relation

/**
  * This is the abstract class for a finite equation system generated by an hyper-graph. Unknowns are nodes
  * of the graph and each hyper-edge has a single target and many possible sources. Given an assignment, each
  * hyper-edge produces a partial values. These values are combined with the upper bound operation.
  */
trait GraphEquationSystem[U, V, E] extends FiniteEquationSystem[U, V] {
  /**
    * The domain type-class for the type `V`.
    */
  val dom: Domain[V]

  /**
    * A function which, given an assignment and an edge, returns the output value of the edge.
    */
  val edgeAction: EdgeAction[U, V, E]

  /**
    * Maps each edge to its source unknowns.
    */
  val sources: E => Iterable[U]

  /**
    * Maps each edge to its target unknown.
    */
  val target: E => U

  /**
    * Maps each unknown to the collection of edges departing from it.
    */
  val outgoing: U => Iterable[E]

  /**
    * Maps each unknown to the collection of edges arriving on it.
    */
  val ingoing: U => Iterable[E]

  /**
    * Add boxes to the equation system in a localized way.
    *
    * @param boxes    new boxes to add.
    * @param ordering an order on unknown used to decide which edges needs to be widened
    */
  def withLocalizedBoxes(boxes: BoxAssignment[U, V], ordering: Ordering[U]): GraphEquationSystem[U, V, E]

  /**
    * Add warrowing to the equation system in a localized way. Localized warrowing requires a different
    * procedure than standard localized widenings. Moreover, it is not entirely clear whether this works as
    * intended or not.
    *
    * @param widenings  a widening assignment
    * @param narrowings a narrowing assignment
    */
  def withLocalizedWarrowing(widenings: BoxAssignment[U, V], narrowings: BoxAssignment[U, V], ordering: Ordering[U]): FiniteEquationSystem[U, V]

  override def withTracer(t: EquationSystemTracer[U, V]): GraphEquationSystem[U, V, E]
}

/**
  * A simple standard implementation of FiniteEquationSystem. All fields must be provided explicitly by
  * the user with the exception of `body`, `bodyWithDependencies` and `infl` which are computed by the
  * graph.
  */
case class SimpleGraphEquationSystem[U, V, E]
(
  unknowns: Iterable[U],
  inputUnknowns: Set[U],
  edgeAction: EdgeAction[U, V, E],
  sources: E => Iterable[U],
  target: E => U,
  outgoing: U => Iterable[E],
  ingoing: U => Iterable[E],
  initial: Assignment[U, V],
  tracer: Option[EquationSystemTracer[U, V]] = None
)(implicit val dom: Domain[V]) extends EquationSystemBase[U, V] with GraphEquationSystem[U, V, E] {

  val body: Body[U, V] = {
    rho: Assignment[U, V] =>
      x: U =>
        tracer foreach (_.beforeEvaluation(rho, x))
        val contributions = for (e <- ingoing(x)) yield edgeAction(rho)(e)
        // if contribution is empty the unknown x has no right hand side... it seems
        // reasonable to return the old value.
        val res = if (contributions.isEmpty)
          rho(x)
        else
          contributions reduce dom.upperBound
        tracer foreach (_.afterEvaluation(rho, x, res))
        res
  }

  override val bodyWithDependencies: BodyWithDependencies[U, V] = {
    rho: Assignment[U, V] =>
      x: U => {
        val deps = ingoing(x).foldLeft(Iterable.empty[U])((acc: Iterable[U], e: E) => acc ++ sources(e))
        val res = body(rho)(x)
        (res, deps)
      }
  }

  val infl: Relation[U] = Relation({
    u: U => (for (e <- outgoing(u)) yield target(e)) (collection.breakOut)
  })

  def withTracer(t: EquationSystemTracer[U, V]): GraphEquationSystem[U, V, E] = {
    copy(tracer = Some(t))
  }

  def withBoxes(boxes: BoxAssignment[U, V]): FiniteEquationSystem[U, V] = {
    val newbody = bodyWithBoxAssignment(boxes)
    val newinfl = if (boxes.boxesAreIdempotent) infl else infl.withDiagonal
    SimpleFiniteEquationSystem(newbody, initial, inputUnknowns, unknowns, newinfl, tracer)
  }

  def withBaseAssignment(init: PartialFunction[U, V])(implicit magma: Magma[V]): FiniteEquationSystem[U, V] = {
    val newbody = bodyWithBaseAssignment(init, magma.op)
    SimpleFiniteEquationSystem(newbody, initial, inputUnknowns, unknowns, infl, tracer)
  }

  def withLocalizedBoxes(boxes: BoxAssignment[U, V], ordering: Ordering[U]): GraphEquationSystem[U, V, E] = {
    val newEdgeAction = {
      rho: Assignment[U, V] =>
        e: E =>
          val x = target(e)
          if (boxes.isDefinedAt(x) && sources(e).exists(ordering.lteq(x, _))) {
            boxes(x)(rho(x), edgeAction(rho)(e))
          } else
            edgeAction(rho)(e)
    }
    if (boxes.boxesAreIdempotent) {
      copy(edgeAction = newEdgeAction)
    } else {
      val newSources = {
        e: E =>
          val x = target(e)
          if (boxes.isDefinedAt(x) && sources(e).exists(ordering.lteq(x, _)))
            sources(e) ++ Iterable(x)
          else
            sources(e)
      }
      val newOutgoing = {
        u: U =>
          if (boxes.isDefinedAt(u)) {
            val edges = ingoing(u).filter {
              e: E => sources(e).exists(ordering.lteq(u, _))
            }
            outgoing(u) ++ edges
          } else
            outgoing(u)
      }
      copy(edgeAction = newEdgeAction, sources = newSources, outgoing = newOutgoing)
    }
  }

  def withLocalizedWarrowing(widenings: BoxAssignment[U, V], narrowings: BoxAssignment[U, V], ordering: Ordering[U]): FiniteEquationSystem[U, V] = {
    val newBody: Body[U, V] = {
      rho: Assignment[U, V] =>
        x: U =>
          val contributions = for (e <- ingoing(x)) yield {
            val contrib = edgeAction(rho)(e)
            val boxapply = sources(e).exists(ordering.lteq(x, _)) && !dom.lteq(contrib, rho(x))
            (contrib, boxapply)
          }
          // if contribution is empty the unknown x has no right hand side... it seems
          // reasonable to return the old value.
          if (contributions.isEmpty)
            rho(x)
          else {
            val result = contributions reduce {
              (x: (V, Boolean), y: (V, Boolean)) => (dom.upperBound(x._1, y._1), x._2 || y._2)
            }
            if (result._2) {
              widenings(x)(rho(x), result._1)
            } else if (dom.lt(result._1, rho(x))) narrowings(x)(rho(x), result._1) else result._1
          }
    }
    val newInfl = if (widenings.boxesAreIdempotent && narrowings.boxesAreIdempotent) infl else infl.withDiagonal
    SimpleFiniteEquationSystem(newBody, initial, inputUnknowns, unknowns, newInfl)
  }
}

object GraphEquationSystem {
  /**
    * Returns the standard implementation of GraphEquationSystem. All fields must be provided explicitly by
    * the user with the exception of `body`, `bodyWithDependencies` and `infl`.
    */
  def apply[U, V: Domain, E]
  (
    unknowns: Iterable[U],
    inputUnknowns: Set[U],
    edgeAction: EdgeAction[U, V, E],
    source: E => Iterable[U],
    target: E => U,
    outgoing: U => Iterable[E],
    ingoing: U => Iterable[E],
    initial: Assignment[U, V]
  ): GraphEquationSystem[U, V, E] =
    SimpleGraphEquationSystem(unknowns, inputUnknowns, edgeAction, source, target, outgoing, ingoing, initial, None)
}
