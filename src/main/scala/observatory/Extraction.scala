package observatory

import java.time.LocalDate
import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val INDEX_STN = 0
    val INDEX_WBAN = 1

    //station line
    val INDEX_LATITUDE = 2
    val INDEX_LONGITUDE = 3

    //temperature line
    val INDEX_MONTH = 2
    val INDEX_DAY = 3
    val INDEX_TEMP = 4
    /**
      *
      * @param f Temperature in degrees Fahrenheit
      * @return Temperature in degrees Celsius (round to one scale)
      */
    def fToC(f: Double) = math.round((f - 32) * 5 / 9 * 10) / 10.0

    /**
      *
      * @param info station: STN	WBAN Latitude	Longitude
      * @return check if the station information is valid, no GPS info means invalid
      */
    def isValidStation(info: String) = info.split(",").length > 3

    val stations = Source.fromInputStream(this.getClass.getResourceAsStream(stationsFile)).getLines
    val validStations = stations.filter(isValidStation)

    val stationLocationMap = validStations.map(line => {
      val info = line.split(",")
      (info(INDEX_STN) + info(INDEX_WBAN)) -> Location(info(INDEX_LATITUDE).toDouble, info(INDEX_LONGITUDE).toDouble)
    }).toMap

    val temperaturesInfo = Source.fromInputStream(this.getClass.getResourceAsStream(temperaturesFile)).getLines
    val tempDateMap = temperaturesInfo.map(line => {
      val info = line.split(",")
      val date = LocalDate.of(year, info(INDEX_MONTH).toInt, info(INDEX_DAY).toInt)
      (info(INDEX_STN) + info(INDEX_WBAN), date, fToC(info(INDEX_TEMP).toDouble))
    })

    val a = for{
      tempDate <- tempDateMap
      if stationLocationMap.keySet.contains(tempDate._1)
    } yield {
      val filtered = stationLocationMap(tempDate._1)
      (tempDate._2, filtered, tempDate._3)
    }
    println("final size " + a.size)
    a.toIterable
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    val a = records.map(x => (x._2, (x._3, 1)))

  }

}
