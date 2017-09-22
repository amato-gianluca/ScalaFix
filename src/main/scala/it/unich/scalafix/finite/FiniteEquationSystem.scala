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
import it.unich.scalafix.lattice.Magma

/**
  * This is the abstract class for an equation system with a finite set of unknowns AND static dependencies between
  * them. When computing `apply(rho)(x)`, the result may only depend on values of `rho(y)` for an `y` such that
  * `y infl x`.
  */
abstract class FiniteEquationSystem[U, V: Magma] extends EquationSystem[U, V] {
  /**
    * The collection of all unknowns.
    */
  val unknowns: Iterable[U]

  /**
    * The static relation between an unknown x and the unknowns y it influences. If `infl(x)` does not contain `y`, it
    * means that `eqs(rho)(y) == eqs(rho')(y)`, when `rho' = rho[x / eqs(rho)(x)]`.
    */
  def infl(u: U): Iterable[U]

  /**
    * Add boxes to the equation system.
    *
    * @param boxes a box assignment.
    */
  def withBoxes(boxes: BoxAssignment[U, V]): FiniteEquationSystem[U, V]

  /**
    * Combine a base assignment with the equation system. The type `V` should be endowed with a magma.
    *
    * @param init the assignment to add to the equation system.
    */
  def withBaseAssignment(init: PartialFunction[U, V]): FiniteEquationSystem[U, V]

  /**
    * Add tracing to the equation system.
    *
    * @param t the tracer to call.
    */
  def withTracer(t: EquationSystemTracer[U,V]): FiniteEquationSystem[U,V]
}

case class SimpleFiniteEquationSystem[U,V: Magma]
( body: Assignment[U,V] => Assignment[U,V],
  bodyInfl: U => Seq[U],
  initial: Assignment[U,V],
  inputUnknowns: U => Boolean,
  unknowns: Iterable[U],
  boxAssignment: BoxAssignment[U,V] = BoxAssignment.empty,
  baseAssignment: PartialFunction[U,V] = PartialFunction.empty,
  tracer: EquationSystemTracer[U,V] = EquationSystemTracer.empty,
) extends FiniteEquationSystem[U,V] with SimpleBodyImpl[U,V] {

  def infl(u: U) = if (boxAssignment.boxesAreIdempotent || ! boxAssignment.isDefinedAt(u))
    bodyInfl(u)
  else
    u +: bodyInfl(u)

  def withBaseAssignment(initial: PartialFunction[U, V]): FiniteEquationSystem[U,V] =
    copy(initial = initial)

  def withBoxes(boxAssignment: BoxAssignment[U, V]): FiniteEquationSystem[U,V] =
    copy(boxAssignment = boxAssignment)

  def withTracer(tracer: EquationSystemTracer[U, V]): FiniteEquationSystem[U,V] =
    copy(tracer = tracer)
}