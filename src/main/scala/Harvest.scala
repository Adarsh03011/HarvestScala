import java.text.SimpleDateFormat
import java.time.Month
import java.util.Date
import scala.collection.mutable.ListBuffer

case class Farmer(val name: String, val date: String,val fruit: String, val quantity: String){

  def getMonth: Int = {
    var spd:SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd")
    var d1: Date = null
    d1 = spd.parse(date)
    d1.getMonth + 1
  }
}

case class Price (fruit:String,date:String,price:String)

object Harvest extends App{

  // Reading Farmers data from csv
  var farmers: ListBuffer[Farmer] = ListBuffer()
  val file = "src/main/resources/harvest.csv"
  val bufferedSource = scala.io.Source.fromFile(file)
  for(lines <- bufferedSource.getLines()){
    val arr = lines.split(",")
    val ans = arr(3)
    val farmer: Farmer = new Farmer(arr(0),arr(1),arr(2),ans)
    farmers.append(farmer)
  }
  bufferedSource.close()

  // Reading Prices data from csv
  var prices: ListBuffer[Price] = ListBuffer()
  val file1 = "src/main/resources/prices.csv"
  val bufferedSource1 = scala.io.Source.fromFile(file1)
  for (lines <- bufferedSource1.getLines()) {
    val arr = lines.split(",")
    val price: Price = new Price(arr(0), arr(1), arr(2))
    prices.append(price)
  }
  bufferedSource1.close()

  farmers = farmers.filter(x => !x.date.equals("2020-01-01"))
  farmers.remove(0) // Removing column header
  prices.remove(0)  // Removing column header
  var farmersNew: List[Farmer] = farmers.toList
  var pricesNew: List[Price] = prices.toList

  //------------------ Best Gatherer in a Month -----------------------------------------------------


  val monthlyBestGatherer = farmersNew.groupBy(h=>(h.name,h.getMonth))
    .map{
      case ((name,month),farmer) =>
        val total = farmer.map(_.quantity.toDouble).sum
        (name,month,total)
    }.groupBy(_._2)
    .map {
      case (month, data) =>
        val (name, _ , total) = data.maxBy(_._3)
        ((month), name , total)
    }
  println("Monthly best gatherer in collecting maximum qunatity")
  monthlyBestGatherer.foreach(x => println(f"${Month.of(x._1)}: {name=${x._2},qty=${x._3}%1.2f}"))

  //--------------------- Gatherer best in collecting specific fruit -------------------------------------------

  val fruitCollection = farmersNew.groupBy(h=>(h.name,h.fruit))
    .map{
      case((name,fruit),farmer) =>
        val total = farmer.map(_.quantity.toDouble).sum
        (name,fruit,total)
    }.groupBy(_._2)
    .map{
      case(fruit,data ) =>
        val (name,_,total) = data.maxBy(_._3)
        (fruit,name,total)
    }

  println("\nGatherer best in collecting specific fruit")
  fruitCollection.foreach(x=>println(f"${x._1}: {name=${x._2},qty=${x._3}%1.2f}" ))

  //--------------------- Overall income of gatherer ---------------------------------------------

  val dayPrice = pricesNew.groupBy(_.fruit)
    .mapValues(_.map(p => (p.date, p.price)).toMap)

  val overallIncome = farmersNew.flatMap{
    f => dayPrice.get(f.fruit).map{
      p => val total = f.quantity.toDouble * p(f.date).toDouble
        (f.name,total)
    }
  }.groupBy(_._1)
    .mapValues(_.map(_._2).sum).toMap
  println("\nOverall Income of each gatherer")
  overallIncome.foreach(x=> println(f"${x._1}: ${x._2}%1.2f"))

  //--------------------- Overall Income from specific fruit -----------------------------------

  val bestFruit = farmersNew.flatMap {
    f =>
      dayPrice.get(f.fruit).map {
        p =>
          val total = f.quantity.toDouble * p(f.date).toDouble
          (f.fruit, total)
      }
  }.groupBy(_._1)
    .mapValues(_.map(_._2).sum).toMap

  println("\nOverall Income from specific fruit")
  bestFruit.foreach(x => println(f"${x._1}: ${x._2}%1.2f"))

  //---------------------- Monthly most profitable fruit  -------------------------


  val monthlyBestFruit = farmersNew.flatMap{
    farmer =>
      dayPrice.get(farmer.fruit).map{price =>
        val earning = (farmer.quantity.toDouble * price(farmer.date).toDouble)
        (farmer.fruit,farmer.getMonth,earning)
      }
  }.groupBy(x=>(x._2,x._1))
    .map{
      case((month,fruit),data) =>
        val pls  = data.map{
          x => x._3
        }.sum
        (month,fruit,pls)
    }.groupBy(_._1).map{
    case(month,data) =>
      val max = data.maxBy(_._3)
      (month,max)
  }.values
  println("\nMonthly Most profitable fruit")
  monthlyBestFruit.foreach(x => println(f"${Month.of(x._1)}: {name=${x._2},amount=${x._3}%1.2f}"))


  // ---------------------------- Monthly least profitable fruit ---------------------------------

  val monthlyLeastProfitableFruit = farmersNew.flatMap {
    farmer =>
      dayPrice.get(farmer.fruit).map { price =>
        val earning = (farmer.quantity.toDouble * price(farmer.date).toDouble)
        (farmer.fruit, farmer.getMonth, earning)
      }
  }.groupBy(x => (x._2, x._1))
    .map {
      case ((month, fruit), data) =>
        val total = data.map {
          x => x._3
        }.sum
        (month, fruit, total)
    }.groupBy(_._1).map {
    case (month, data) =>
      val min = data.minBy(_._3)
      (month, min)
  }.values
  println("\nMonthly Most profitable fruit")
  monthlyLeastProfitableFruit.foreach(x => println(f"${Month.of(x._1)}: {name=${x._2},amount=${x._3}%1.2f}"))

  // ----------------------------- Monthly best earning by gatherer ------------------------------------

  val monthlyBestEarner = farmersNew.flatMap{
    farmer =>
      dayPrice.get(farmer.fruit).map{ price =>
        val earning = (farmer.quantity.toDouble * price(farmer.date).toDouble)
        (farmer.name,farmer.getMonth,earning)
      }
  }.groupBy(x => (x._2,x._1))
    .map{
      case((month,name),data) =>
        val total = data.map{
          x => x._3
        }.sum
        (month,name,total)
    }.groupBy(_._1).map{
    case(month,data) =>
      val bestEarner = data.maxBy(_._3)
      (month,bestEarner)
  }.values
  println("\nMonthly best gatherer")
  monthlyBestEarner.foreach(x => println(f"${Month.of(x._1)}: {name=${x._2},amount=${x._3}%1.2f}"))




}
