package observatory

import java.io.File

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  //  def acos(x: Float): Float = {
  //    val negate : Float = float(x < 0)
  //    val abs_x = math.abs(x)
  //    var ret : Float = -0.0187293F
  //    ret = ret * abs_x
  //    ret = ret + 0.0742610F
  //    ret = ret * abs_x
  //    ret = ret - 0.2121144F
  //    ret = ret * abs_x
  //    ret = ret + 1.5707288F
  //    ret = ret * (math.sqrt(1.0 - abs_x)).toFloat
  //    ret = ret - 2 * negate * ret
  //    negate * 3.14159265358979F + ret
  //  }

  class LocMath(loc: Location) {
    val ratio = 57.2958

    import org.apache.commons.math3.util.FastMath._

    val sin_lat = sin(loc.lat / ratio)
    val cos_lat = cos(loc.lat / ratio)
    val sin_lon = sin(loc.lon / ratio)
    val cos_lon = cos(loc.lon / ratio)
  }

  //(Location, sin_lat, cos_lat, sin_lon, cos_lon)
  var locMathMap = Map[Location, LocMath]()

  def distanceToLoc(a: Location, loc: Location): Double = {
        /**
          * get locMath object for location a and add to locMathMap
          *
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

        import org.apache.commons.math3.util.FastMath._
        val bigC = sqrt(pow(x, 2) + pow(y, 2) + pow(z, 2))
        val angle = asin(bigC / 2) * 2
        angle * 6371
//    import org.apache.commons.math3.util.FastMath._
//    val ratio = 57.2958
//    acos(sin(a.lat / ratio) * sin(loc.lat / ratio) + cos(a.lat / ratio) * cos(loc.lat / ratio) * cos(abs(loc.lon / ratio - a.lon / ratio))) * 6371
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    // distance: (temp, dist)
    import org.apache.commons.math3.util.FastMath._
    temperatures.find(_._1.equals(location)) match {
      case Some(x) => x._2
      case None => {
        val k = temperatures.par
          .map(x => (x._2, distanceToLoc(x._1, location)))

        if (k.exists(_._2 < 1)) k.filter(_._2 < 1).toArray.apply(0)._1
        else {
          val mm = if(k.exists(_._2 < 3000)) k.filter(_._2 < 3000) else k
          val q = mm.map(x => (pow(1 / x._2, 8), x._1))
            .map(x => (x._1 * x._2, x._1))
            .reduce((a, b) => (a._1 + b._1, a._2 + b._2))
          q._1 / q._2
        }
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

    import org.apache.commons.math3.util.FastMath._
    val locationArray = new Array[(Int, Int)](Lat_MAX * Lon_MAX)
    // (Location, temp)
    val pairs = temperatures.map(x => (Location(round(x._1.lat * 10.0) / 10.0,  round(x._1.lon * 10.0) / 10.0), x._2))

    for (lat <- -89 to 90; lon <- -180 to 179) {
      locationArray((lat + 89) * 360 + (lon + 180)) = (1 - lat, lon)
    }

    val pixelArray = locationArray.par
      .map(x => predictTemperature(pairs, Location(x._1, x._2)))
      .map(x => interpolateColor(colors, x))
      .map(x => Pixel(x.red, x.green, x.blue, 127))
      .toArray


    val image = Image(Lon_MAX, Lat_MAX, pixelArray, 2)
  //  image.output(new File("/home/saga/IdeaProjects/Capstone_Project/target/mage.png"))
                image.output(new File("target/mage.png"))
    image

  }
}

