package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("locateTemperatures") {
    assert(Extraction.locateTemperatures(2000, "/stations.csv", "/2000.csv").nonEmpty)
  }

//    test("locationYearlyAverageRecords") {
//      assert(Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")).nonEmpty)
//    }
}