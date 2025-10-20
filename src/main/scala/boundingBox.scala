// Jack Mulvihill

package edu.luc.cs.laufer.cs371.shapes

// Imports all case constructors from the shape enum into current scope
import Shape.*
import com.typesafe.scalalogging.LazyLogging

// Computes the axis-aligned bounding box of a shape
object boundingBox extends LazyLogging:
  def apply(s: Shape): Location = 
    logger.debug(s"Computing bounding box for: $s")
    
    // The result will always be a Location containing a Rectangle, added location annotation.
    val result: Location = s match
      case Rectangle(w, h) =>
        logger.debug(s"Rectangle case: width=$w, height=$h")
        Location(0, 0, Rectangle(w, h))
      
      case Ellipse(semiWidth, semiHeight) =>
        logger.debug(s"Ellipse case: semiWidth=$semiWidth, semiHeight=$semiHeight")

        // Ellipse parameters are semi-axes (radii), so bounding box has full dimensions 2w × 2h
        // Centered at origin, extends semiWidth in each x direction, semiHeight in each y direction
        val fullWidth = 2 * semiWidth
        val fullHeight = 2 * semiHeight
        val x = -semiWidth
        val y = -semiHeight
        logger.debug(s"Ellipse bounding box: x=$x, y=$y, width=$fullWidth, height=$fullHeight")
        Location(x, y, Rectangle(fullWidth, fullHeight))
      
      case Location(x, y, shape) =>
        logger.debug(s"Location case: x=$x, y=$y, shape=$shape")
        // Get bounding box of inner shape and translate it
        val Location(innerX, innerY, Rectangle(innerW, innerH)) = apply(shape)
        Location(x + innerX, y + innerY, Rectangle(innerW, innerH))
      
      case Group(shapes*) =>
        logger.debug(s"Group case with ${shapes.length} shapes")
        if shapes.isEmpty then
          logger.debug("Empty group, returning zero bounding box")
          Location(0, 0, Rectangle(0, 0))
        else
          // Compute bounding boxes for all shapes
          val boxes = shapes.map(apply)
          
          logger.debug(s"Computed boxes: $boxes")
          
          // Extract coordinates of all corners
          val leftCoords = boxes.map { case Location(x, _, Rectangle(_, _)) => x }
          val rightCoords = boxes.map { case Location(x, _, Rectangle(w, _)) => x + w }
          val topCoords = boxes.map { case Location(_, y, Rectangle(_, _)) => y }
          val bottomCoords = boxes.map { case Location(_, y, Rectangle(_, h)) => y + h }
          
          // Find extremes
          val minX = leftCoords.min
          val maxX = rightCoords.max
          val minY = topCoords.min
          val maxY = bottomCoords.max
          
          val boundingWidth = maxX - minX
          val boundingHeight = maxY - minY
          
          logger.debug(s"Group bounding box: x=$minX, y=$minY, width=$boundingWidth, height=$boundingHeight")
          Location(minX, minY, Rectangle(boundingWidth, boundingHeight))
    
    logger.debug(s"Result: $result")
    result

end boundingBox