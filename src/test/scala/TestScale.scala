package edu.luc.cs.laufer.cs371.shapes

import org.scalatest.funsuite.AnyFunSuite

import TestFixtures.*
import Shape.*

class TestScale extends AnyFunSuite:

  def testScale(description: String, s: Shape, factor: Double, expected: Shape): Unit =
    test(description):
      val result = scale(s, factor)
      assert(expected == result, s"Expected $expected but got $result")

  // Test basic leaf shapes with different scale factors
  testScale("simple ellipse scale 2.0", 
    simpleEllipse, 
    2.0, 
    Ellipse(100, 60))
  
  testScale("simple ellipse scale 0.5", 
    simpleEllipse, 
    0.5, 
    Ellipse(25, 15))
  
  testScale("simple rectangle scale 2.0", 
    simpleRectangle, 
    2.0, 
    Rectangle(160, 240))
  
  testScale("simple rectangle scale 0.5", 
    simpleRectangle, 
    0.5, 
    Rectangle(40, 60))
  
  // Test Location scaling (scales both position and shape), see statements below for details
  testScale("simple location scale 2.0", 
    simpleLocation, 
    2.0, 
    Location(140, 60, Rectangle(160, 240)))
  
  testScale("simple location scale 0.5", 
    simpleLocation, 
    0.5, 
    Location(35, 15, Rectangle(40, 60)))
  
  // Test Group scaling
  testScale("basic group scale 2.0", 
    basicGroup, 
    2.0, 
    Group(Ellipse(100, 60), Rectangle(40, 80)))
  
  testScale("basic group scale 0.5", 
    basicGroup, 
    0.5, 
    Group(Ellipse(25, 15), Rectangle(10, 20)))
  
  testScale("simple group scale 2.0", 
    simpleGroup, 
    2.0, 
    Group(
      Location(400, 200, Ellipse(100, 60)),
      Location(800, 600, Rectangle(200, 100))
    ))
  
  testScale("simple group scale 0.5", 
    simpleGroup, 
    0.5, 
    Group(
      Location(100, 50, Ellipse(25, 15)),
      Location(200, 150, Rectangle(50, 25))
    ))
  
  testScale("complex group scale 2.0", 
    complexGroup, 
    2.0,
    Location(100, 200,
      Group(
        Ellipse(40, 80),
        Location(300, 100,
        Group(
          Rectangle(100, 60),
          Rectangle(600, 120),
          Location(200, 400,
            Ellipse(100, 60)
          )
        )),
        Rectangle(200, 400)
      )))
  
  // Additional test cases, see statements below for details
  testScale("identity scale 1.0 rectangle", 
    Rectangle(100, 200), 
    1.0, 
    Rectangle(100, 200))
  
  testScale("identity scale 1.0 ellipse", 
    Ellipse(50, 30), 
    1.0, 
    Ellipse(50, 30))
  
  testScale("empty group scale 2.0", 
    Group(), 
    2.0, 
    Group())
  
  testScale("nested locations scale 3.0", 
    Location(10, 20, Location(30, 40, Rectangle(50, 60))), 
    3.0,
    Location(30, 60, Location(90, 120, Rectangle(150, 180))))
  
  testScale("group with mixed shapes scale 0.25",
    Group(
      Rectangle(20, 40),
      Ellipse(60, 80),
      Location(100, 200, Rectangle(10, 20))
    ),
    0.25,
    Group(
      Rectangle(5, 10),
      Ellipse(15, 20),
      Location(25, 50, Rectangle(2, 5))
    ))
  
  testScale("nested groups scale 1.5",
    Group(
      Rectangle(10, 20),
      Group(Ellipse(20, 30), Rectangle(40, 50))
    ),
    1.5,
    Group(
      Rectangle(15, 30),
      Group(Ellipse(30, 45), Rectangle(60, 75))
    ))
  
  testScale("scale zero produces zero-sized shapes",
    Rectangle(100, 200),
    0.0,
    Rectangle(0, 0))
  
  testScale("scale large factor",
    Ellipse(5, 10),
    10.0,
    Ellipse(50, 100))
  
  testScale("scale with decimal factor",
    Rectangle(100, 100),
    0.75,
    Rectangle(75, 75))
  
  testScale("complex nested structure scale 0.5",
    Location(20, 40,
      Group(
        Rectangle(60, 80),
        Location(100, 120,
          Group(
            Ellipse(30, 40),
            Rectangle(50, 60)
          )
        )
      )
    ),
    0.5,
    Location(10, 20,
      Group(
        Rectangle(30, 40),
        Location(50, 60,
          Group(
            Ellipse(15, 20),
            Rectangle(25, 30)
          )
        )
      )
    ))
  
  testScale("scale with negative coordinates",
    Location(-50, -100, Rectangle(20, 40)),
    2.0,
    Location(-100, -200, Rectangle(40, 80)))
  
  testScale("group with multiple levels scale 3.0",
    Group(
      Location(10, 10, Ellipse(5, 5)),
      Group(
        Location(20, 20, Rectangle(10, 10)),
        Location(30, 30, Ellipse(15, 15))
      )
    ),
    3.0,
    Group(
      Location(30, 30, Ellipse(15, 15)),
      Group(
        Location(60, 60, Rectangle(30, 30)),
        Location(90, 90, Ellipse(45, 45))
      )
    ))

end TestScale