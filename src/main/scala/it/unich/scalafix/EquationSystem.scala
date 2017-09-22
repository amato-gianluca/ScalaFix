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
  * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix

import scala.collection.mutable
import it.unich.scalafix.lattice.Magma

/**
  * This is the abstract class for a generic equation system.
  *
  * @tparam U the type for the unknowns.
  * @tparam V the type for the values assumed by the unknowns.
  */
abstract class EquationSystem[U, V](implicit val magma: Magma[V]) {
  /**
    * Given an assignment and an unknown, compute the new value of the unknown.
    */
  def apply(rho: Assignment[U, V], u: U): V

  /**
    * Given an assignment and an unknown, returns a pair `(apply(rho, u), uks)`. `uks` is a set of unknowns
    * with the property that if `rho'` differs from `rho` only for variables which are not in `uks`, then
    * `applu(rho,u)==apply(rho',u)`.
    */
  def applyWithDependencies(rho: Assignment[U, V], u: U): (V, Iterable[U])

  /**
    * An initial value for starting the analyzer
    */
  val initial: Assignment[U, V]

  /**
    * The unknowns which may be considered the input to this equation system.
    */
  val inputUnknowns: U => Boolean

  /**
    * Add boxes to the equation system.
    *
    * @param boxes a box assignment.
    */
  def withBoxes(boxes: BoxAssignment[U, V]): EquationSystem[U, V]

  /**
    * Combine a base assignment with the equation system. The type `V` should be endowed with a magma.
    *
    * @param init the assignment to add to the equation system.
    */
  def withBaseAssignment(init: PartialFunction[U, V]): EquationSystem[U, V]

  /**
    * Add tracing to the equation system.
    *
    * @param t the tracer to call.
    */
  def withTracer(t: EquationSystemTracer[U, V]): EquationSystem[U, V]
}

trait SimpleBodyImpl[U,V] extends EquationSystem[U,V] {
  val body: Assignment[U, V] => Assignment[U, V]
  val boxAssignment: BoxAssignment[U, V]
  val baseAssignment: PartialFunction[U, V]
  val tracer: EquationSystemTracer[U, V]

  def apply(rho: Assignment[U, V], u: U): V = {
    tracer.preEvaluation(rho, u)
    val res = if (baseAssignment.isDefinedAt(u))
      body(rho)(u)
    else
      magma.op(body(rho)(u), baseAssignment(u))
    tracer.postEvaluation(rho, u, res)
    if (boxAssignment.isDefinedAt(u)) {
      val boxedRes = boxAssignment(u)(rho(u), res)
      tracer.boxEvaluation(rho, u, res, boxedRes)
      boxedRes
    } else {
      tracer.noBoxEvaluation(rho, u, res)
      res
    }
  }

  def applyWithDependencies(rho: Assignment[U, V], u: U): (V, Iterable[U]) = {
    val queried = mutable.Buffer.empty[U]
    val trackrho = { y: U =>
      queried.append(y)
      rho(y)
    }
    val newval = apply(trackrho, u)
    (newval, queried)
  }
}

case class SimpleEquationSystem[U, V: Magma]
(
  body: Assignment[U, V] => Assignment[U, V],
  initial: Assignment[U, V],
  inputUnknowns: U => Boolean,
  boxAssignment: BoxAssignment[U, V] = BoxAssignment.empty,
  baseAssignment: PartialFunction[U, V] = PartialFunction.empty,
  tracer: EquationSystemTracer[U, V] = EquationSystemTracer.empty
) extends SimpleBodyImpl[U, V] {

  def withBaseAssignment(initial: PartialFunction[U, V]) = copy(initial = initial)

  def withBoxes(boxAssignment: BoxAssignment[U, V]) = copy(boxAssignment = boxAssignment)

  def withTracer(tracer: EquationSystemTracer[U, V]) = copy(tracer = tracer)
}
