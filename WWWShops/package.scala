import WWWShops.wwwEtsyShop

object Main extends App {
  //run settings
    val saveToFile: Boolean = false
    val runFromFile: Boolean = false
    val debugPrint: Boolean = false

  //init Etsy
    val myEtsyStore = wwwEtsyShop(saveToFile, runFromFile, debugPrint)

  //Execute Etsy proc to get results
    val EtsyData = myEtsyStore.runShop

  //print out the results
    myEtsyStore.printOutResults()
}

