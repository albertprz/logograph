// package init

// import org.apache.spark.sql.functions._
// import org.apache.spark.sql.types._
// import org.apache.spark.SparkConf
// import org.apache.spark.sql._

import scala.reflect._
import scala.concurrent.ExecutionContext.Implicits.global
import java.sql.Date


// object SparkExample extends App {

//   def getClassMemberNames [T: ClassTag] () = classTag[T]
//                                           .runtimeClass
//                                           .getDeclaredFields()
//                                           .map(_.getName)

//   case class CovidData (continent: Option[String], country: Option[String],
//                         county: Option[String], province: Option[String],
//                         deaths: Option[Long], deathsDelta: Option[Long],
//                         positiveCases: Option[Long], positiveCasesDelta: Option[Long],
//                         dataSource: Option[String], date: Option[Date])

//   val columnsToSelect = List (
//     "CONTINENT_NAME", "COUNTRY_SHORT_NAME", "COUNTY_NAME", "PROVINCE_STATE_NAME",
//     "PEOPLE_DEATH_COUNT", "PEOPLE_DEATH_NEW_COUNT", "PEOPLE_POSITIVE_CASES_COUNT",
//     "PEOPLE_POSITIVE_NEW_CASES_COUNT", "DATA_SOURCE_NAME", "REPORT_DATE"
//   )


//   val spark = SparkSession
//     .builder()
//     .master("local[*]")
//     .getOrCreate()

//   import spark.implicits._

//   val sc = spark.sparkContext

//   val jsonData = spark.read.json("C:\\Users\\i23449\\Downloads\\covid-data.json")
//   val columns = jsonData
//     .select(columnsToSelect.head, columnsToSelect.tail : _*)
//     .toDF(getClassMemberNames[CovidData]: _*)
//     .withColumn("date", to_date(col("date"), "MM/dd/yyyy"))
//     .as[CovidData]


//   columns
//     .filter(_.dataSource.exists(_ == "European Centre for Disease Prevention and Control"))
//     .filter(_.continent.exists(_ == "Europe" ))
//     .filter(_.date.exists(_.getMonth >= 3))
//     .groupBy("continent", "country")
//     .agg("deaths" → "mean", "positiveCases" → "sum")
//     .withColumnRenamed("sum(deaths)", "deaths")
//     .withColumnRenamed("sum(positiveCases)", "positive-cases")
//     .sort($"deaths".desc, $"positive-cases".desc)
//     .show()

//   Thread.sleep(120000)
//   sc.stop()
// }
