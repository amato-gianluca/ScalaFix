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

package it.unich.jandom.fixpoint.lattice

/**
 * A DirectedSet is a partially ordered set where each pair of elements has an upper bound.
 */
trait DirectedSet[+A] extends PartiallyOrdered[A] {
  /**
   * It returns an upper bound of `x` and `y`.
   */
  def union[B >: A <% PartiallyOrdered[B]](that: B): B
  
  /**
   * It returns an upper bound of `x` and `y`. It is an alias for the `upperbound` method.
   */
  @inline def \/[B >: A <% PartiallyOrdered[B]](that: B) = this union that
}
