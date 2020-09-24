
import org.apache.spark.sql.SparkSession
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.apache.spark.sql.functions._

import org.apache.spark.sql.Column
import org.apache.spark.sql.catalyst.expressions.aggregate.ApproximatePercentile
import org.apache.spark.sql.functions.lit
import org.apache.spark.SparkContext
// import org.apache.spark.mllib.random.RandomRDDs._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.functions.sqrt
import org.apache.spark.sql.expressions.Window
import org.apache.spark.sql.SaveMode


object rjack {

	def main(args: Array[String])
	{

	args.foreach(println)	
	
	
	val spark = SparkSession.builder().appName("rjack_outliers").getOrCreate()
	val sc = spark.sparkContext
	
	import spark.implicits._
	spark.sparkContext.setLogLevel("ERROR") // reduce printed output 
	
	object PercentileApprox {
	  def percentile_approx(col: Column, percentage: Column, accuracy: Column): Column = {
		val expr = new ApproximatePercentile(
		  col.expr,  percentage.expr, accuracy.expr
		).toAggregateExpression
		new Column(expr)
	  }
	  def percentile_approx(col: Column, percentage: Column): Column = percentile_approx(
		col, percentage, lit(ApproximatePercentile.DEFAULT_PERCENTILE_ACCURACY)
	  )
	}
	import PercentileApprox._
	
	/////////////////////////////////////////
	// import bioclim data 
	/////////////////////////////////////////
	val table_name = "bioclim_0.1_extract.tsv" // this bioclim table is produced using bioclim_extract.r
	val database = "prod_h"
	// val database = "uat"

	// read in bioclim table   
	val df_bioclim = spark.read.
	option("sep", "\t").
	option("header", "true").
	option("inferSchema", "true").
	csv(table_name).
	withColumn("decimallatitude_bioclim",$"decimallatitude").
	withColumn("decimallongitude_bioclim",$"decimallongitude").
	drop("decimallatitude").
	drop("decimallongitude")
  
	/////////////////////////////////////////
	// import occurrence data 
	/////////////////////////////////////////
	val df_original = spark.sql("SELECT * FROM " + database + ".occurrence").
	filter($"hasgeospatialissues" === false).
	filter($"basisofrecord" =!= "FOSSIL_SPECIMEN").
	filter($"basisofrecord" =!= "UNKNOWN").
	filter($"basisofrecord" =!= "LIVING_SPECIMEN"). 
	filter($"kingdomkey" === 1 || $"kingdomkey" === 6 || $"kingdomkey" === 5). 
	filter($"decimallatitude".isNotNull).
	filter($"decimallongitude".isNotNull).
	withColumn("rounded_decimallatitude", round(col("decimallatitude") * 100 / 10) * 10 / 100).
	withColumn("rounded_decimallongitude", round(col("decimallongitude") * 100 / 10) * 10 / 100). 
	select(
	"gbifid",
	"datasetkey",
	"basisofrecord",
	"specieskey",
	"genuskey",
	"familykey",
	"orderkey",
	"phylumkey",
	"classkey",
	"kingdomkey",
	"species",
	"genus",
	"family",
	"phylum",
	"order_",
	"class",
	"kingdom",
	"decimallatitude",
	"decimallongitude",
	"rounded_decimallatitude",
	"rounded_decimallongitude",
	"coordinateuncertaintyinmeters"
	)
  
	// get distinct coordinates 
	val df_occ_1 = df_original. 
	select("specieskey","rounded_decimallatitude","rounded_decimallongitude"). 
	distinct()

	val df_bioclim_join = df_occ_1.join(df_bioclim, 
	df_bioclim("decimallatitude_bioclim") === df_occ_1("rounded_decimallatitude") &&
	df_bioclim("decimallongitude_bioclim") === df_occ_1("rounded_decimallongitude"))

	// put data into long format to run all 19
	val df_occ_2 = df_bioclim_join. 
	selectExpr("specieskey","rounded_decimallatitude","rounded_decimallongitude","decimallatitude_bioclim","decimallongitude_bioclim", "stack(23, 'bio1', bio1, 'bio2', bio2,'bio3', bio3,'bio4', bio4,'bio5', bio5,'bio6', bio6,'bio7', bio7,'bio8', bio8,'bio9', bio9,'bio10', bio10,'bio11', bio11,'bio12', bio12,'bio13', bio13,'bio14', bio14,'bio15', bio15,'bio16', bio16,'bio17', bio17,'bio18', bio18,'bio19', bio19)").
	withColumnRenamed("col0","bioclim").
	withColumnRenamed("col1","xx").
	withColumn("specieskey_bioclim",concat($"specieskey", lit("_"), $"bioclim")).
	select("specieskey_bioclim","xx").
	distinct()
  
	val distinct_counts = df_occ_2.
	groupBy("specieskey_bioclim").
	count()

	/////////////////////////////////////////
	// begin reverse jackknife part 
	/////////////////////////////////////////
	val windowSpec = Window.partitionBy("specieskey_bioclim").orderBy($"xx")

	val df_1 = df_occ_2.join(distinct_counts,"specieskey_bioclim").
	filter($"count" >= 10). // only species with more than 10 bioclim values 
	select("specieskey_bioclim","xx").
	withColumn("x1", lead("xx", 1) over windowSpec).
	orderBy("specieskey_bioclim","xx")

	val df_constants = df_1.
	groupBy("specieskey_bioclim").
	agg(
	mean(col("xx")).alias("mx"),
	count(lit(1)).alias("n")).
	withColumn("n1",$"n" - 1).
	withColumn("t1",(lit(0.95) * sqrt($"n")) + 0.2)

	val df_2 = df_1.join(df_constants,"specieskey_bioclim").
	withColumn("y",
	when(col("xx") < col("mx"), 
	($"x1" - $"xx") * ($"mx" - $"xx")).
	otherwise(
	($"x1" - $"xx") * ($"x1" - $"mx"))
	)

	val df_my = df_2.
	groupBy("specieskey_bioclim").
	agg(mean($"y").alias("my"))

	val df_3 = df_2.join(df_my,"specieskey_bioclim")

	val df_sqrt_sum = df_3.
	withColumn("diff",$"y" - $"my").
	withColumn("pow",pow($"diff",lit(2))).
	groupBy("specieskey_bioclim","n1").
	agg(sum($"pow").alias("sum")). 
	withColumn("sqrt_sum_n1",sqrt($"sum"/$"n1")). 
	select("specieskey_bioclim","sqrt_sum_n1")

	val df_z = df_3.join(df_sqrt_sum,"specieskey_bioclim").
	withColumn("z",$"y"/($"sqrt_sum_n1")).
	withColumnRenamed("specieskey_bioclim","specieskey_bioclim_z").
	withColumnRenamed("xx","xx_z").
	select("specieskey_bioclim_z","xx_z","z")

	val df_4 = df_3.join(df_z, 
	df_z("specieskey_bioclim_z") === df_3("specieskey_bioclim") &&
	df_z("xx_z") === df_3("xx")).
	drop("xx_z"). 
	select("specieskey_bioclim","xx","t1","z").
	withColumn("v",
	when(col("z") > col("t1"),$"xx").
	otherwise(null)
	)

	val df_quantiles = df_4.groupBy($"specieskey_bioclim").
	agg(percentile_approx($"xx", typedLit(Seq(0.25,0.5,0.75))).as("quantiles")).
	select($"*",
	$"quantiles".getItem(0).as("Q1"),
	$"quantiles".getItem(1).as("median_x"),
	$"quantiles".getItem(2).as("Q3")
	).
	select("specieskey_bioclim","median_x")

	val df_5 = df_4.join(df_quantiles,"specieskey_bioclim").
	withColumn("greater",$"v" > $"median_x").
	withColumn("less",$"v" < $"median_x").
	withColumn("outlier_greater", ($"greater" && $"xx" >= $"v") ).
	withColumn("outlier_lesser", ($"less" && $"xx" <= $"v") ).
	orderBy("specieskey_bioclim","xx").
	cache()

	val df_lower = df_5.
	filter($"outlier_lesser").
	groupBy("specieskey_bioclim").
	agg(min("xx").alias("min_xx"))

	val df_upper = df_5.
	filter($"outlier_greater").
	groupBy("specieskey_bioclim").
	agg(max("xx").alias("max_xx"))

	val df_outliers_upper = df_5.join(df_upper,"specieskey_bioclim").
	filter($"xx" >= $"max_xx").
	withColumn("outlier_type",lit("upper")).
	select("specieskey_bioclim","xx","outlier_type").
	cache()

	val df_outliers_lower = df_5.join(df_lower,"specieskey_bioclim").
	filter($"xx" <= $"min_xx").
	withColumn("outlier_type",lit("lower")).
	select("specieskey_bioclim","xx","outlier_type").
	cache()

	val outliers = Seq(df_outliers_upper,df_outliers_lower).
	reduce(_ union _)
	
	/////////////////////////////////////////
	// save temp table outliers 
	/////////////////////////////////////////
	import org.apache.spark.sql.SaveMode
	import sys.process._

	val save_temp_name = "rjack_outliers_temp"

	outliers.
	write.format("csv").
	option("sep", "\t").
	option("header", "true").
	mode(SaveMode.Overwrite).
	save(save_temp_name)

	//////////////////////////////////////////
	// merge back with original data 
	/////////////////////////////////////////

	val df_outliers = spark.read.
	option("sep", "\t").
	option("header", "true").
	option("inferSchema", "true").
	csv(save_temp_name)

	// clean up and merge back with original data to get gbifids 
	val df_bio_gbifid = df_original.join(df_bioclim, 
	df_bioclim("decimallatitude_bioclim") === df_original("rounded_decimallatitude") &&
	df_bioclim("decimallongitude_bioclim") === df_original("rounded_decimallongitude")). 
	selectExpr("gbifid","specieskey", "stack(23, 'bio1', bio1, 'bio2', bio2,'bio3', bio3,'bio4', bio4,'bio5', bio5,'bio6', bio6,'bio7', bio7,'bio8', bio8,'bio9', bio9,'bio10', bio10,'bio11', bio11,'bio12', bio12,'bio13', bio13,'bio14', bio14,'bio15', bio15,'bio16', bio16,'bio17', bio17,'bio18', bio18,'bio19', bio19)").
	withColumnRenamed("col0","bioclim").
	withColumnRenamed("col1","xx").
	withColumn("specieskey_bioclim",concat($"specieskey", lit("_"), $"bioclim")).
	withColumnRenamed("specieskey_bioclim","specieskey_bioclim_source").
	withColumnRenamed("xx","xx_source").
	select("gbifid","specieskey_bioclim_source","xx_source")

	val df_export_1 = df_outliers.join(df_bio_gbifid,
	df_outliers("xx") === df_bio_gbifid("xx_source") &&
	df_outliers("specieskey_bioclim") === df_bio_gbifid("specieskey_bioclim_source")).
	drop("xx_source").
	drop("specieskey_source").
	drop("decimallatitude_bioclim").
	select("gbifid","specieskey_bioclim","xx").
	withColumn("tmp", split($"specieskey_bioclim", "_")).
	withColumn("specieskey",$"tmp".getItem(0)).
	withColumn("bioclim",$"tmp".getItem(1)).
	select("gbifid","specieskey","bioclim","xx")

	// count the number of bioclim outliers for gbifid
	val df_n_bioclim_outliers = df_export_1.
	filter($"bioclim".contains("bio")). // filter out pca variables (comp1,comp2...)
	groupBy("gbifid").
	agg(countDistinct("bioclim").as("n_bioclim_outliers"))

	// pivot back to long format 
	val df_pivoted = df_export_1.
	groupBy("gbifid","specieskey").
	pivot("bioclim").
	agg(first(col("xx")))

	val df_export_2 = df_pivoted.join(df_n_bioclim_outliers,"gbifid").
	drop("specieskey")

	val df_export = df_original.join(df_export_2,"gbifid")
	
	println("----------------- final count ---------------------") 
	println(df_export.count())
	
	// save outliers /////
	import org.apache.spark.sql.SaveMode
	import sys.process._

	val save_table_name = "rjack_outliers_export"

	df_export.
	write.format("csv").
	option("sep", "\t").
	option("header", "false").
	mode(SaveMode.Overwrite).
	save(save_table_name)
	
	// scp -r /cygdrive/c/Users/ftw712/Desktop/gbif_reverse_jackknife/target/scala-2.11/gbif_reverse_jackknife_2.11-0.1.jar jwaller@c5gateway-vh.gbif.org:/home/jwaller/
	// spark2-submit --num-executors 40 --executor-cores 5 --driver-memory 8g --driver-cores 4 --executor-memory 16g gbif_reverse_jackknife_2.11-0.1.jar


	
  }

}
	