package edu.luc.cs.laufer.cs371.shapes

// Imports all case constructors from the shape enum into current scope
import Shape.*
import com.typesafe.scalalogging.LazyLogging

// Recursively scales a shape by a given factor
object scale extends LazyLogging:
  def apply(s: Shape, factor: Double): Shape =
    logger.debug(s"Scaling shape by factor $factor: $s")
    val result = s match
      case Rectangle(w, h) =>
        val scaledWidth = (w * factor).toInt
        val scaledHeight = (h * factor).toInt
        val scaled = Rectangle(scaledWidth, scaledHeight)
        logger.debug(s"Scaled Rectangle: $w x $h -> $scaledWidth x $scaledHeight")
        scaled
      
      case Ellipse(w, h) =>
        val scaledWidth = (w * factor).toInt
        val scaledHeight = (h * factor).toInt
        val scaled = Ellipse(scaledWidth, scaledHeight)
        logger.debug(s"Scaled Ellipse: $w x $h -> $scaledWidth x $scaledHeight")
        scaled
      
      case Location(x, y, shape) =>
        // Scale both coordinates and the inner shape
        val scaledX = (x * factor).toInt
        val scaledY = (y * factor).toInt
        val scaled = Location(scaledX, scaledY, apply(shape, factor))
        logger.debug(s"Scaled Location: ($x, $y) -> ($scaledX, $scaledY)")
        scaled
      
      case Group(shapes*) =>
        // Recursively scale all shapes in the group
        val scaledShapes = shapes.map(apply(_, factor))
        logger.debug(s"Scaled Group with ${shapes.length} shapes")
        Group(scaledShapes*)
    
    logger.debug(s"Scale result: $result")
    result

end scale