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

    //concatenate STN and WBAN to be an comparable id(tuple is not easy to compare), add # to avoid coincidence
    val SEPERATOR = "#"
    /**
      *
      * @param f Temperature in degrees Fahrenheit
      * @return Temperature in degrees Celsius (round to one scale)
      */
    def fToC(f: Double) = math.round((f - 32) * 5 / 9 * 10) / 10.0

    def extract(line: String) = {
      val info = line.split(",").toList
      (info.head + SEPERATOR + info.tail.head, info.tail.tail)
    }
    /**
      *
      * @param info station: STN	WBAN Latitude	Longitude
      * @return check if the station information is valid, no GPS info means invalid
      */
    def isValidStation(info: String) = info.split(",").length > 3

    val stations = Source.fromInputStream(this.getClass.getResourceAsStream(stationsFile)).getLines
    //Map(stationId -> (Latitude, Longitude))
    val stationLocationPair = stations.filter(isValidStation).map(extract).toMap

    val temperaturesInfo = Source.fromInputStream(this.getClass.getResourceAsStream(temperaturesFile)).getLines
    //(stationId, (Month, Day, Temp))
    val dateTempPair = temperaturesInfo.map(extract)

    val a = for {
      dateTemp <- dateTempPair
      if stationLocationPair.keySet.contains(dateTemp._1)
    } yield {
      val locInfo = stationLocationPair(dateTemp._1)
      val location = Location(locInfo.head.toDouble, locInfo(1).toDouble)

      val date = LocalDate.of(year, dateTemp._2.head.toInt, dateTemp._2(1).toInt)

      (date, location, dateTemp._2(2).toDouble)
    }
    a.toIterable
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    val locTemGroup = records.map(x => (x._2, x._3, 1)).groupBy(_._1)
    val sumTemp = locTemGroup.mapValues(_.reduce((a, b)=>(a._1, a._2 + b._2, a._3 + b._3)))
    sumTemp.map(x => (x._1, math.round((x._2._2 / x._2._3) * 10.0) / 10.0))
  }
}
