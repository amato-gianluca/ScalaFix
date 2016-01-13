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
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.fixpoint

import scala.language.higherKinds

/**
 * This is the common trait of all fixpoint solvers for equation systems. It is just a marker trait.
 * All fixpoint solvers have different an apply method (with different parameters) which may be used
 * to solve an equation system.
 */
trait FixpointSolver {
  
  /**
   * The base class from which all the parameter types should descend.
   */
  abstract protected class BaseParams[U,V] {
    /**
     * The initial assignment to start the analysis
     */
    val start: Assignment[U,V]
    
    /**
     * A listener for debugging or tracing purposes
     */
    val listener: FixpointSolverListener[U,V]
  }
  
  /**
   * Each fixpoint solver needs an equation system and some parameters. Parameters are generally different
   * for each fixpoint solver, hence they are provided as an inner type. 
   */
  type Params[U,V] <: BaseParams[U,V]
  
  /**
   * EQS is the subclass of equation systems the solver may work with.
   */
  type EQS[U,V] <: EquationSystem[U,V]
}
