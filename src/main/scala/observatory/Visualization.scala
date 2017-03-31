package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  class LocMath(loc: Location) {
    val radio = 57.2958 // 1 radius = 1 degree * radio
    val sin_lat = math.sin(loc.lat / radio)
    val cos_lat = math.cos(loc.lat / radio)
    val sin_lon = math.sin(loc.lon / radio)
    val cos_lon = math.cos(loc.lon / radio)
  }

  // (Location, sin_lat, cos_lat, sin_lon, cos_lon)
  var locMathMap = Map[Location, LocMath]()

  def distanceToLoc(a: Location, loc: Location): Double = {
    /**
      * get locMath object for location a and add to locMathMap
      * @param a location
      * @return generated locMath object
      */
    def getLocMath(a: Location) = locMathMap.get(a) match {
      case Some(x) => x
      case None => {
        val element = new LocMath(a)
        locMathMap += (a -> element)
        element
      }
    }

    val tempA = getLocMath(a)
    val tempLoc = getLocMath(loc)

    // A 1, Loc 2
    val x = tempLoc.cos_lat * tempLoc.cos_lon - tempA.cos_lat * tempA.cos_lon
    val y = tempLoc.cos_lat * tempLoc.sin_lon - tempA.cos_lat * tempA.sin_lon
    val z = tempLoc.sin_lat - tempA.sin_lat

    val bigC = math.sqrt(math.pow(x, 2) + math.pow(y, 2) + math.pow(z, 2))
    math.asin(bigC / 2)
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {

    // distance: (temp, dist)
    temperatures.find(_._1.equals(location)) match {
      case Some(x) => x._2
      case None => {
        val k = temperatures
          .map(x => (x._2, distanceToLoc(x._1, location)))
          .map(x => (1 / x._2, x._1))
          .map(x => (x._1 * x._2, x._1))
          .reduce((a, b) => (a._1 + b._1, a._2 + b._2))
        k._1 / k._2
      }
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {

    /**
      * implementation of Linear Interpolation
      *
      * @param y0 color value of lower point
      * @param x0 color zone lower boundary
      * @param y1 color value of higher point
      * @param x1 color zone higher boundary
      * @return
      */
    def calculateRGB(y0: Double, x0: Double, y1: Double, x1: Double): Int = {
      val ratio = (value - x0) / (x1 - x0)
      val color = y0 * (1 - ratio) + y1 * ratio
      math.round(color).toInt
    }

    val color = points.toList.sortWith((a, b) => a._1 > b._1)
    val colorPair = color.zip(color.tail)

    value match {
      case _ if value >= color.head._1 => color.head._2
      case _ if value <= color.last._1 => color.last._2
      case _ => {
        val colorZone = colorPair.find(x => x._1._1 > value && value >= x._2._1).get
        // bigger
        val n = colorZone._1
        // smaller
        val m = colorZone._2
        val r = calculateRGB(m._2.red, m._1, n._2.red, n._1)
        val g = calculateRGB(m._2.green, m._1, n._2.green, n._1)
        val b = calculateRGB(m._2.blue, m._1, n._2.blue, n._1)
        Color(r, g, b)
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val Lat_MAX = 180
    val Lon_MAX = 360
    val imageArray = Array.ofDim[Pixel](Lat_MAX, Lon_MAX)

    // (Location, temp)
    val pairs = temperatures.map(x => (Location(math.round(x._1.lat * 10.0) / 10.0, math.round(x._1.lon * 10.0) / 10.0), x._2))

    for (lat <- -89 to 90; lon <- -180 to 179) {
        val preTemp = predictTemperature(pairs, Location(1 - lat, lon))
        val preColor = interpolateColor(colors, preTemp)
        imageArray(lat + 89)(lon + 180) = Pixel(preColor.red, preColor.green, preColor.blue, 127)
    }
    val image = Image(Lon_MAX, Lat_MAX, imageArray.flatten, 2)
//    image.output(new java.io.File("/home/saga/Workspace/coursera/scala/Capstone_Project/observatory/target/mage.png"))
    image.output(new java.io.File("target/mage.png"))
    image
  }
}

