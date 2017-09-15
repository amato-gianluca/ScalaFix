/**
  * Copyright 2017 Gianluca Amato <gianluca.amato@unich.it>
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

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks

class BodyTest extends FunSpec with PropertyChecks {
  val increment: Body[Int, Int] = { rho: Assignment[Int, Int] => { u: Int => rho(u) + 1 } }

  describe("the identity body") {
    it("behaves like the identity function") {
      val id = Body.identity[Int, Int]
      forAll { (rho: Assignment[Int, Int]) =>
        assertResult(rho)(id(rho))
      }
    }
    it("is the same object regardless its type") {
      val id1 = Body.identity[Int, Int]
      val id2 = Body.identity[Double, String]
      assert(id1 == id2)
    }
  }

  describe("the bodyWithBaseAssignment") {
    it("combines a body with a base assignment") {
      val rho: PartialFunction[Int, Int] = {
        case x => x
      }
      val newbody = increment.withBaseAssignment(rho, { (x: Int, y: Int) => x + y })
      forAll { (rho: Assignment[Int, Int], x: Int) =>
        assertResult(rho(x) + x + 1)(newbody(rho)(x))
      }
    }
  }

  describe("the bodyWithBoxAssignment") {
    it("combines a body with a box assignment") {
      val box: Box[Int] = {
        _ + _
      }
      val boxes = BoxAssignment(box).restrict((x: Int) => x % 2 == 0)
      val newbody = increment.withBoxAssignment(boxes)
      forAll { (rho: Assignment[Int, Int], x: Int) =>
        val y = 2 * x
        assertResult(rho(y) + 1 + rho(y))(newbody(rho)(y))
        val z = 2 * x + 1
        assertResult(rho(z) + 1)(newbody(rho)(z))
      }
    }

    it("returns the original body if the box assinment is empty") {
      val boxes = BoxAssignment.empty[Int]
      val newbody = increment.withBoxAssignment(boxes)
      assert(increment == newbody)
    }
  }

  describe("a body") {
    it("is a SAM class") {
      val increment = { rho: Assignment[Int, Int] => { u: Int => rho(u) + 1 } }
      forAll { (rho: Assignment[Int, Int], x: Int) =>
        assertResult(rho(x) + 1)(increment(rho)(x))
      }
    }

    it("may be explicitly created with the Body compaion object") {
      val increment = Body { rho: Assignment[Int, Int] => { u: Int => rho(u) + 1 } }
      forAll { (rho: Assignment[Int, Int], x: Int) =>
        assertResult(rho(x) + 1)(increment(rho)(x))
      }
    }
  }

}