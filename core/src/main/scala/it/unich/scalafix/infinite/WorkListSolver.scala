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
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with ScalaFix.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.scalafix.infinite

import it.unich.scalafix._
import it.unich.scalafix.assignments.{IOAssignment, InputAssignment}

import scala.collection.mutable

/**
  * A local fixpoint solver based on a worklist.
  */
object WorkListSolver {
  /**
    * Locally solve a finite equation system.
    *
    * @tparam U type of the unknowns for the equation system
    * @tparam V type of values of the equatiom system
    * @param eqs    equation system to solve
    * @param wanted the unknowns we want to solve
    * @param start  assignment to start the evaluation (defaults to `eqs.initial`)
    * @param tracer a tracer to track the behaviour of the solver (defaults to the empty tracer)
    * @return the solution of the equation system
    */
  def apply[U, V](eqs: EquationSystem[U, V])
                 (
                   wanted: Iterable[U],
                   start: InputAssignment[U, V] = eqs.initial,
                   tracer: FixpointSolverTracer[U, V] = FixpointSolverTracer.empty[U, V]
                 ): IOAssignment[U, V] = {
    val infl: mutable.MultiMap[U, U] = new mutable.HashMap[U, mutable.Set[U]] with mutable.MultiMap[U, U] {
      override def makeSet = new mutable.LinkedHashSet[U]
    }
    val workList = mutable.Queue.empty[U]
    workList ++= wanted

    val current = start.toIOAssignment
    tracer.initialized(current)
    while (workList.nonEmpty) {
      val x = workList.dequeue()
      val (newval, dependencies) = eqs.bodyWithDependencies(current)(x)
      tracer.evaluated(current, x, newval)
      for (y <- dependencies) {
        if (!current.isDefinedAt(y)) {
          current(y) = start(y)
          workList += y
        }
        infl.addBinding(y, x)
      }
      if (newval != current(x)) {
        current(x) = newval
        workList ++= infl(x)
      }
    }
    tracer.completed(current)
    current
  }
}
