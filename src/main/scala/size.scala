package edu.luc.cs.laufer.cs371.shapes

// Imports all case constructors from the shape enum into current scope
import Shape.*
import com.typesafe.scalalogging.LazyLogging

// Counts the number of concrete leaf shapes (such as ellipses and rectangles) in a general shape
object size extends LazyLogging:
  def apply(s: Shape): Int =
    logger.debug(s"Computing size for: $s")
    val result = s match
      case Rectangle(_, _) =>
        logger.debug("Rectangle leaf node: count=1")
        1
      
      case Ellipse(_, _) =>
        logger.debug("Ellipse leaf node: count=1")
        1
      
      case Location(_, _, shape) =>
        logger.debug("Location node: recursing into inner shape")
        apply(shape)
      
      case Group(shapes*) =>
        logger.debug(s"Group node with ${shapes.length} shapes")
        // Sum the sizes of all shapes in the group
        val totalSize = shapes.map(apply).sum
        logger.debug(s"Group total size: $totalSize")
        totalSize
    
    logger.debug(s"Size result: $result")
    result

end size