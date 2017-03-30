package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  val white = (60.0, Color(255, 255, 255))
  val red = (32.0, Color(255, 0, 0))
  val yellow = (12.0, Color(255, 255, 0))
  val sky = (0.0, Color(0, 255, 255))
  val blue = (-15.0, Color(0, 0, 255))
  val violet = (-27.0, Color(255, 0, 255))
  val purple = (-50.0, Color(33, 0, 107))
  val black = (-60.0, Color(0, 0, 0))


  test("creation of image") {

    //val color = Array(white, red, yellow, sky, blue, violet, purple, black)

//    val locTemps = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(2000, "/stations.csv", "/2000.csv"))

    val loc = Location(88.0,-176.0)
  //  location lat 88.0 lon -176.0 temp 16.230242849423977
    val a = Array((Location(45.0,-90.0), 10.0), (Location(-45.0,0.0), 20.0))

    println(Visualization.predictTemperature(a, loc))
//    Visualization.visualize(locTemps, color.toIterable)
  }
}



































