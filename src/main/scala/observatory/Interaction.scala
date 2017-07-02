package observatory

import java.io.File

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x    X coordinate
    * @param y    Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val n = math.pow(2, zoom)
    val lon = x / n * 360 - 180
    import org.apache.commons.math3.util.FastMath._
    val lat = atan(sinh(math.Pi * (1 - y * 2 / n))) * 180 / math.Pi
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param zoom         Zoom level
    * @param x            X coordinate
    * @param y            Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val TILE_SIZE = 256
    val LAT_MAX = 85.0511
    val LAT_MIN = -85.0511
    import org.apache.commons.math3.util.FastMath._
    val zoomRatio = pow(2, zoom)
    val LatStep = 170.1022 / zoomRatio / TILE_SIZE
    val LonStep = 360 / zoomRatio / TILE_SIZE

    val locationArray = new Array[Location](TILE_SIZE * TILE_SIZE)
    val topLeftLocation = tileLocation(zoom, x, y)

    for (lat <- 0 until TILE_SIZE; lon <- 0 until TILE_SIZE) {
      locationArray(lat * TILE_SIZE + lon) =
        Location(topLeftLocation.lat - LatStep * lat, topLeftLocation.lon + LonStep * lon)
    }

    val pixelArray = locationArray
      .map(x => if (x.lat > LAT_MAX) Location(LAT_MAX, x.lon)
      else if (x.lat < LAT_MIN) Location(LAT_MIN, x.lon) else x)
      .par.map(x => Visualization.predictTemperature(temperatures, x))
      .map(x => Visualization.interpolateColor(colors, x))
      .map(x => Pixel(x.red, x.green, x.blue, 127))
      .toArray

    val image = Image(TILE_SIZE, TILE_SIZE, pixelArray, 2)
   // image.output(new File("/home/saga/IdeaProjects/Capstone_Project/target/temperatures/" + zoom + "/" + x + "-" + y + ".png"))
    image.output(new File("target/mage.png"))
    image
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Int, Data)],
                           generateImage: (Int, Int, Int, Int, Data) => Unit
                         ): Unit = {
    ???
  }

}
