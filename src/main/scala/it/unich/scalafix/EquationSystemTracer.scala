/**
  * Copyright 2016, 2017 Gianluca Amato <gianluca.amato@unich.it>
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

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION

/**
  * A BodyTrace is a class with many entry-points which are called by the applyWithTracer method
  * of the Body class. May be used for debugging, measuring performances, etc...
  *
  * @tparam U the type of unknowns
  * @tparam V the type of values
  */
abstract class EquationSystemTracer[U,V] {
  /**
    * This method is called before the body is evaluated.
    * @param rho the input assignment of the body
    * @param u the unknown to be evaluated
    */
  @elidable(ASSERTION)
  def preEvaluation(rho: Assignment[U,V], u: U)

  /**
    * This method is called after the body is evaluated.
    * @param rho the input assignment of the body
    * @param u the unknown to be evaluated
    * @param res result of the body evaluation
    */
  @elidable(ASSERTION)
  def postEvaluation(rho: Assignment[U,V], u: U, res: V)

  /**
    * This method is called after the body and box is evaluted.
    * @param rho the input assignment of the body
    * @param u the unknown to be evaluated
    * @param res result of the evaluation of the original body
    * @param boxed result of the evaluation of the original body, boxed with the original value
    */
  @elidable(ASSERTION)
  def boxEvaluation(rho: Assignment[U,V], u: U, res: V, boxed: V)

  /**
    * This method is called after the body is evaluated, when no box is evaluated.
    * @param rho the input assignment of the body
    * @param u the unknown to be evaluated
    * @param res result of the evaluation of the original body
    */
  @elidable(ASSERTION)
  def noBoxEvaluation(rho: Assignment[U,V], u: U, res: V)
}

/**
  * A BodyTracer which does nothing.
  *
  * @tparam U the type of unknowns
  * @tparam V the type of values
  */
class EquationSystemTracerEmpty[U, V] extends EquationSystemTracer[U,V] {
  def preEvaluation(rho: Assignment[U, V], u: U): Unit = {}

  def postEvaluation(rho: Assignment[U, V], u: U, res: V): Unit = {}

  def boxEvaluation(rho: Assignment[U, V], u: U, res: V, boxed: V): Unit = {}

  def noBoxEvaluation(rho: Assignment[U, V], u: U, res: V): Unit = {}
}

/**
  * A BodyTracer which prints debugging information.
  *
  * @tparam U the type of unknowns
  * @tparam V the type of values
  */
class EquationSystemTracerDebug[U,V] extends EquationSystemTracer[U,V] {
  def preEvaluation(rho: Assignment[U,V], u: U): Unit = {
    println(s"Evaluating body at unknown ${u}, old value: ${rho(u)}")
  }

  def postEvaluation(rho: Assignment[U,V], u: U, res: V): Unit = {
    println(s"Evaluated body at unknown ${u}, old value: ${rho(u)}, new value: ${res}")
  }

  def boxEvaluation(rho: Assignment[U,V], u: U, res: V, boxed: V): Unit = {
    println(s"Evaluated body at unknown ${u}, old value: ${rho(u)}, new value ${res}, boxed ${res}")
  }

  def noBoxEvaluation(rho: Assignment[U,V], u: U, res: V): Unit = {
    println(s"No box evaluated at unknown  ${u}, old value: ${rho(u)}, new value ${res}")
  }
}


object EquationSystemTracer {
  /**
    * Returns a tracer which does nothing.
    */
  def empty[U, V] = new EquationSystemTracerEmpty[U, V]

  /**
    * Returns a tracer which prints a lot of debugging information.
    */
  def debug[U, V] = new EquationSystemTracerDebug[U, V]
}
