package edu.luc.cs.laufer.cs371.shapes

import org.scalatest.funsuite.AnyFunSuite

import TestFixtures.*
import Shape.*

class TestSize extends AnyFunSuite:

  def testSize(description: String, s: Shape, expected: Int): Unit =
    test(description):
      val result = size(s)
      assert(expected == result, s"Expected size $expected but got $result")

  // Test basic leaf shapes (size = 1)
  testSize("simple ellipse", simpleEllipse, 1)
  testSize("simple rectangle", simpleRectangle, 1)
  
  // Test Location wrapper (doesn't add to count, just wraps)
  testSize("simple location", simpleLocation, 1)
  
  // Test Groups
  testSize("basic group", basicGroup, 2)
  testSize("simple group", simpleGroup, 2)
  testSize("complex group", complexGroup, 5)
  
  // Additional edge cases, see statements below for details
  testSize("empty group", Group(), 0)
  
  testSize("nested locations", 
    Location(10, 20, Location(30, 40, Rectangle(50, 60))), 
    1)
  
  testSize("group with only rectangles", 
    Group(Rectangle(10, 20), Rectangle(30, 40), Rectangle(50, 60)), 
    3)
  
  testSize("group with only ellipses", 
    Group(Ellipse(10, 20), Ellipse(30, 40), Ellipse(50, 60)), 
    3)
  
  testSize("group with mixed leaf shapes", 
    Group(Rectangle(10, 20), Ellipse(30, 40), Rectangle(50, 60)), 
    3)
  
  testSize("group with locations wrapping leaves",
    Group(
      Location(10, 20, Rectangle(30, 40)),
      Location(50, 60, Ellipse(70, 80)),
      Location(90, 100, Rectangle(110, 120))
    ), 
    3)
  
  testSize("nested groups",
    Group(
      Rectangle(10, 20),
      Group(Ellipse(30, 40), Rectangle(50, 60)),
      Ellipse(70, 80)
    ), 
    4)
  
  testSize("deeply nested with locations and groups",
    Location(0, 0,
      Group(
        Location(10, 10,
          Group(
            Rectangle(20, 20),
            Location(30, 30, Ellipse(40, 40))
          )
        ),
        Rectangle(50, 50)
      )
    ), 
    3)
  
  testSize("large group",
    Group(
      Rectangle(10, 20),
      Ellipse(30, 40),
      Location(50, 60, Rectangle(70, 80)),
      Group(
        Ellipse(90, 100),
        Rectangle(110, 120)
      ),
      Location(130, 140, 
        Group(
          Rectangle(150, 160),
          Ellipse(170, 180),
          Rectangle(190, 200)
        )
      )
    ),
    8)
  
  testSize("multiple nesting levels with same total",
    Group(
      Group(Rectangle(10, 20), Ellipse(30, 40)),
      Group(Rectangle(50, 60), Ellipse(70, 80))
    ),
    4)

end TestSize