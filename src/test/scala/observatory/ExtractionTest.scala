package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("locateTemperatures") {
    Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
  }

  
}