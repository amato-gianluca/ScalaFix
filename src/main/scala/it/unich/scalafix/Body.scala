/**
  * Copyright 2016 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of ScalaFix.
  * ScalaFix is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * ScalaFix is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix

import it.unich.scalafix.Body.{BodyWithBaseAssignment, BodyWithBoxAssignment}
import it.unich.scalafix.lattice.Magma

/**
  * This is the type for the body of an equation system, i.e., a function which takes an assignment of values
  * to unknowns and returns a new assignment of values to unknowns.
  *
  * @tparam U the type of the unknowns
  * @tparam V the type of values associated to the unknowns
  */
abstract class Body[U, V] extends Function[Assignment[U, V], Assignment[U, V]] {
  /**
    * This method takes a body and a box assignment (as a template) and returns a new body where boxes have been
    * plugged inside, i.e. `newbody(rho)(x) = boxes(x) ( rho(x), body(rho)(x) )`.
    */
  def withBoxAssignment(boxes: BoxAssignment[U, V]): Body[U, V] =
    if (boxes.isEmpty)
      this
    else
      new BodyWithBoxAssignment(this, boxes)

  /**
    * Given a partial assignment, returns a new equation system where the r.h.s. of each unknown is combined
    * with the assignment. The magma operator is used for the combination. In formulas, if `newbody` is the result of
    * the method and `init` is defined on 'x', then `newbody(rho)(x) = init(x) op body(rho)(x)`.
    *
    * @param init the initial partial assignment, given as a partial function
    */
  def withBaseAssignment(init: PartialFunction[U, V])(implicit magma: Magma[V]): Body[U, V] =
    new BodyWithBaseAssignment(this, init, magma.op)

  /**
    * Given a partial assignment, returns a new equation system where the r.h.s. of each unknown is combined
    * with the assignment. The comb operator is used for the combination. In formulas, if `newbody` is theresult of
    * the method and `init` is defined on 'x', then `newbody(rho)(x) = init(x) comb body(rho)(x)`.
    *
    * @param init the initial partial assignment, given as a partial function
    * @param comb the function use to combine the body with partial function
    */
  def withBaseAssignment(init: PartialFunction[U, V], comb: (V, V) => V): Body[U, V] =
    new BodyWithBaseAssignment(this, init, comb)
}

/**
  * The `Body` object defines several factories for building bodies.
  */
object Body {

  private object IdentityBody extends Body[Any, Any] {
    def apply(rho: Assignment[Any, Any]) = rho
  }

  private final class BodyWithBoxAssignment[U, V](body: Body[U, V], boxes: BoxAssignment[U, V]) extends Body[U, V] {
    val realBoxes = boxes.copy

    def apply(rho: Assignment[U, V]) = { (x: U) =>
      if (realBoxes.isDefinedAt(x)) realBoxes(x)(rho(x), body(rho)(x)) else body(rho)(x)
    }
  }

  private final class BodyWithBaseAssignment[U, V](body: Body[U, V], init: PartialFunction[U, V], comb: (V, V) => V) extends Body[U, V] {
    def apply(rho: Assignment[U, V]) = { (x: U) =>
      if (init.isDefinedAt(x)) comb(init(x), body(rho)(x)) else body(rho)(x)
    }
  }

  private final class BodyFromFunction[U, V](f: Assignment[U, V] => Assignment[U, V]) extends Body[U, V] {
    def apply(rho: Assignment[U, V]) = f(rho)
  }

  /**
    * Returns the identity body, i.e., the one which returns the same assignment given as input.
    *
    * @tparam U the tyep of unknowns
    * @tparam V the type of values
    */
  def identity[U, V]: Body[U, V] = IdentityBody.asInstanceOf[Body[U, V]]

  /**
    * A body given by a function `f` between assignments.
    *
    * @param f function from assignment to assignment.
    */
  def apply[U, V](f: Assignment[U, V] => Assignment[U, V]): Body[U, V] =
    new BodyFromFunction(f)
}
