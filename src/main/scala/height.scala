package edu.luc.cs.laufer.cs371.shapes

// Imports all case constructors from the shape enum into current scope
import Shape.*
import com.typesafe.scalalogging.LazyLogging

// Computes the height of the shape tree taking all kinds of shape nodes into consideration
object height extends LazyLogging:
  def apply(s: Shape): Int =
    logger.debug(s"Computing height for: $s")
    val result = s match
      case Rectangle(_, _) =>
        logger.debug("Rectangle leaf node: height=0")
        0
      
      case Ellipse(_, _) =>
        logger.debug("Ellipse leaf node: height=0")
        0
      
      case Location(_, _, shape) =>
        logger.debug("Location node: height = 1 + inner height")
        // Location adds one level to the tree height
        1 + apply(shape)
      
      case Group(shapes*) =>
        logger.debug(s"Group node with ${shapes.length} shapes")
        if shapes.isEmpty then
          logger.debug("Empty group: height=0")
          0
        else
          // Height of group is 1 + max height of children
          val maxChildHeight = shapes.map(apply).max
          val groupHeight = 1 + maxChildHeight
          logger.debug(s"Group height: $groupHeight (1 + max child height $maxChildHeight)")
          groupHeight
    
    logger.debug(s"Height result: $result")
    result

end height