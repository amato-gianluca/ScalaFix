/**
  * Copyright 2015, 2016 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of ScalaFix.
  * ScalaFix is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * ScalaFix is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix.finite

import it.unich.scalafix._
import it.unich.scalafix.lattice.Domain
import it.unich.scalafix.utils.Relation

/**
  * This is the abstract class for a finite equation system generated by an hyper-graph. Unknowns are nodes
  * of the graph and each hyper-edge has a single target and many possible sources. Given an assignment, each
  * hyper-edge produces a partial values. These values are combined with the upper bound operation.
  */
abstract class GraphEquationSystem[U, V, E](implicit val dom: Domain[V]) extends FiniteEquationSystem[U, V] {
  /**
    * A function which, given an assignment and edge, returns the output value of
    * the edge.
    */
  val edgeAction: Assignment[U, V] => E => V

  /**
    * Maps each edge to its sources unknown.
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
    * @param boxes    new box to add.
    * @param ordering an order on unknown in order to decide which edges needs to be widened
    */
  def withLocalizedBoxes(boxes: BoxAssignment[U, V], ordering: Ordering[U]): GraphEquationSystem[U, V, E]

  /**
    * Add warrowing to the equation system in a localized way. Localized warrowing requires a different
    * procedure than standard localized widenings. Moreover, it is not entirely clear whether this works as
    * intended or not.
    *
    * @param widenings  a widenings assignment
    * @param narrowings a narrowings assignment
    */
  def withLocalizedWarrowing(widenings: BoxAssignment[U, V], narrowings: BoxAssignment[U, V], ordering: Ordering[U]): FiniteEquationSystem[U, V]
}

object GraphEquationSystem {

  /**
    * A mixin for GraphEquationSystem which implements the body from the edgeAction method.
    */
  trait BodyFromEdgeAction[U, V, E] {
    this: GraphEquationSystem[U, V, E] =>

    val body = new Body[U, V] {
      def apply(rho: Assignment[U, V]) = {
        (x: U) =>
          val contributions = for (e <- ingoing(x)) yield edgeAction(rho)(e)
          // if contribution is empty the unknown x has no right hand side... it seems
          // reasonable to return the old value.
          if (contributions.isEmpty)
            rho(x)
          else
            contributions reduce dom.upperBound
      }
    }
  }

  /**
    * A mixin for GraphEquationSystem which implements the methods `withLocalizedBoxes` and
    * `withLocalizedWarrowing`.
    */
  trait WithLocalizedBoxes[U, V, E] {
    self: GraphEquationSystem[U, V, E] =>

    def withLocalizedBoxes(boxes: BoxAssignment[U, V], ordering: Ordering[U]): GraphEquationSystem[U, V, E] = {
      val newEdgeAction = { (rho: Assignment[U, V]) =>
        e: E =>
          val x = target(e)
          if (boxes.isDefinedAt(x) && sources(e).exists(ordering.lteq(x, _))) {
            boxes(x)(rho(x), edgeAction(rho)(e))
          } else
            edgeAction(rho)(e)
      }
      if (boxes.boxesAreIdempotent) {
        new SimpleGraphEquationSystem[U, V, E](unknowns, inputUnknowns, newEdgeAction, sources, target, outgoing, ingoing, initial) {
          val bodyWithDependencies = self.bodyWithDependencies
          val infl = self.infl
        }
      } else {
        val newSource = { (e: E) =>
          val x = target(e)
          if (boxes.isDefinedAt(x) && sources(e).exists(ordering.lteq(x, _)))
            sources(e) ++ Iterable(x)
          else
            sources(e)
        }
        val newOutgoing = { (x: U) =>
          if (boxes.isDefinedAt(x)) {
            val edges = ingoing(x).filter { (e: E) => sources(e).exists(ordering.lteq(x, _)) }
            outgoing(x) ++ edges
          } else
            outgoing(x)
        }
        new SimpleGraphEquationSystem[U, V, E](unknowns, inputUnknowns, newEdgeAction, newSource, target, newOutgoing, ingoing, initial)
          with ComputedDependencies[U, V, E]
      }
    }

    def withLocalizedWarrowing(widenings: BoxAssignment[U, V], narrowings: BoxAssignment[U, V], ordering: Ordering[U]): FiniteEquationSystem[U, V] = {
      val newbody = new Body[U, V] {
        def apply(rho: Assignment[U, V]) = {
          (x: U) =>
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
              val result = contributions reduce { (x: (V, Boolean), y: (V, Boolean)) => (dom.upperBound(x._1, y._1), x._2 || y._2) }
              //println((x, rho(x), contributions))
              if (result._2) {
                widenings(x)(rho(x), result._1)
              } else if (dom.lt(result._1, rho(x))) narrowings(x)(rho(x), result._1) else result._1
            }
        }
      }

      FiniteEquationSystem(
        body = newbody,
        initial = initial,
        inputUnknowns = inputUnknowns,
        unknowns = unknowns,
        infl = if (widenings.boxesAreIdempotent && narrowings.boxesAreIdempotent) infl else infl.withDiagonal
      )
    }

  }

  /**
    * A mixin for GraphEquationSystem which implements bodyFromDependecies and infl from the graph structure.
    */
  trait ComputedDependencies[U, V, E] {
    this: GraphEquationSystem[U, V, E] =>

    val bodyWithDependencies = {
      rho: Assignment[U, V] =>
        x: U => {
          val deps = ingoing(x).foldLeft(Iterable.empty[U])((acc: Iterable[U], e: E) => acc ++ sources(e))
          val res = body(rho)(x)
          (res, deps)
        }
    }

    val infl: Relation[U] = Relation({ (u: U) =>
      (for (e <- outgoing(u)) yield target(e)) (collection.breakOut)
    })
  }

  /**
    * An implementation of `GraphEquationSystem` from a subset of its constituents.
    */
  abstract class SimpleGraphEquationSystem[U, V: Domain, E](
                                                             val unknowns: Iterable[U],
                                                             val inputUnknowns: Set[U],
                                                             val edgeAction: Assignment[U, V] => E => V,
                                                             val sources: E => Iterable[U],
                                                             val target: E => U,
                                                             val outgoing: U => Iterable[E],
                                                             val ingoing: U => Iterable[E],
                                                             val initial: Assignment[U, V]
                                                           )
    extends GraphEquationSystem[U, V, E] with FiniteEquationSystem.WithBaseAssignment[U, V]
      with FiniteEquationSystem.WithBoxes[U, V] with BodyFromEdgeAction[U, V, E] with WithLocalizedBoxes[U, V, E]

  /**
    * Returns an implementation of a `GraphEquationSystem` from a subset of its constituents.
    */
  def apply[U, V: Domain, E](
                              unknowns: Iterable[U],
                              inputUnknowns: Set[U],
                              edgeAction: Assignment[U, V] => E => V,
                              source: E => Iterable[U],
                              target: E => U,
                              outgoing: U => Iterable[E],
                              ingoing: U => Iterable[E],
                              initial: Assignment[U, V]
                            ): GraphEquationSystem[U, V, E] =
    new SimpleGraphEquationSystem(unknowns, inputUnknowns, edgeAction, source, target, outgoing, ingoing, initial) with
      ComputedDependencies[U, V, E]
}
