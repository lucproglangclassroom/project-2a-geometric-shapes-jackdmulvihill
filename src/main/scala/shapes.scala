// Jack Mulvihill

package edu.luc.cs.laufer.cs371.shapes

/** data Shape = Rectangle(w, h) | Location(x, y, Shape) */
enum Shape derives CanEqual:
  case Rectangle(width: Int, height: Int)

  // Added ellipse case class to the Shape enum 
  case Ellipse(width: Int, height: Int)
  
  case Location(x: Int, y: Int, shape: Shape)
  case Group(shapes: Shape*)