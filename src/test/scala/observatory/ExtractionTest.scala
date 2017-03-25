package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("locateTemperatures") {
    Extraction.locateTemperatures(2000, "/stations.csv", "/2000.csv")
  }

  test("locationYearlyAverageRecords") {
    Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv"))
  }
}