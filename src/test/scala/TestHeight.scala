// Jack Mulvihill

package edu.luc.cs.laufer.cs371.shapes

import org.scalatest.funsuite.AnyFunSuite

import TestFixtures.*
import Shape.*

class TestHeight extends AnyFunSuite:

  def testHeight(description: String, s: Shape, expected: Int): Unit =
    test(description):
      val result = height(s)
      assert(expected == result, s"Expected height $expected but got $result")

  // Test basic leaf shapes (height = 0)
  testHeight("simple ellipse", simpleEllipse, 0)
  testHeight("simple rectangle", simpleRectangle, 0)
  
  // Test Location wrapper (adds 1 to height)
  testHeight("simple location", simpleLocation, 1)
  
  // Test Groups
  testHeight("basic group", basicGroup, 1)
  testHeight("simple group", simpleGroup, 2)
  testHeight("complex group", complexGroup, 5)
  
  // Additional edge cases, see statements below for details
  testHeight("empty group", Group(), 0)
  
  testHeight("nested locations", 
    Location(10, 20, Location(30, 40, Rectangle(50, 60))), 
    2)
  
  testHeight("group with only leaves", 
    Group(Rectangle(10, 20), Ellipse(30, 40), Rectangle(50, 60)), 
    1)
  
  testHeight("group with one location", 
    Group(
      Rectangle(10, 20),
      Location(30, 40, Ellipse(50, 60))
    ), 
    2)
  
  testHeight("deeply nested structure",
    Location(0, 0,
      Group(
        Location(10, 10,
          Group(
            Location(20, 20, 
              Rectangle(30, 30)
            )
          )
        )
      )
    ), 
    5)
  
  testHeight("group with mixed nesting depths",
    Group(
      Rectangle(10, 20),                              // height 0
      Location(30, 40, Ellipse(50, 60)),             // height 1
      Location(70, 80, 
        Location(90, 100, Rectangle(110, 120))       // height 2
      )
    ),
    3)  // 1 (group) + max(0, 1, 2) = 1 + 2 = 3

end TestHeight