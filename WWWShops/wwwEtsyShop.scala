package WWWShops

import java.io.{BufferedWriter, FileWriter}
import org.json4s.native.JsonMethods._
import org.json4s.JObject
import scala.io.Source
import java.nio.file.{Paths, Files}

case class wwwEtsyShop(saveToFile: Boolean, runFromFile: Boolean, debugPrint: Boolean) extends wwwShop {
  override val shopName = "Etsy"
  override val shopNameShort = "Etsy"

  override type shopJsonType = List[Map[String,String]]

  //the folloving vars should be taken from config structure (registry, database, config file, etc.)
  val rootJason = "results"
  val api_key = "1lkwp2nik08gdczk8mk9172l"
  val etsyURLformat = "https://openapi.etsy.com/v2//shops/%s/listings/active?api_key=%s&fields=title,description"
  //val localWorkDir: String = localShopWorkDir + shopNameShort + "\\"
  val localWorkDir: String = localShopWorkDir

  if ((saveToFile || runFromFile) && !Files.exists(Paths.get(localWorkDir))) {
    throw new Exception("ERROR: Path does not exist: %s".format(localWorkDir))
  }

  //shop ids for fetching the data from API
  val shop_ids: List[storeIdType] =
    List("Aspeleshoes", "MeenalPatelStudio", "twoems", "VintageEbookArchive",
    "MyPersonalMemories", "GentleSateen", "OldTownSpiceShop",
    "socksandmittens", "sustainablemaria", "Charlie7Bags",
      "WhateverStrangeStore")   //Last store is fake for demo

  //construct url to visit
  def getURLs: urlCollectionType = {
    shop_ids.map(sid => (sid, String.format(etsyURLformat, sid, api_key)))
  }

  def getWordCounts(sid:String,
                    jRes:shopJsonType,    //json type specific to the shop web api
                    oneStoreWords: List[storeWords]): List[storeWords] = jRes match {
    case Nil => oneStoreWords
    case res :: tail =>
      val title = res("title").toLowerCase().replaceAll("\\W", " ")
      val description = res("description").toLowerCase().replaceAll("\\W", " ")

      //words in title count twice to get double weight
      val words = title + " " + title + " " + description

      //aggregate all words with counts
      val all_words: Map[String, Int] = words
        .split(' ')
        .filter(x => x.length() > 2 && !delete_words.contains(x))
        .groupMapReduce(x => x) (_ => 1) (_ + _)

      getWordCounts(sid,tail,storeWords(sid,all_words) :: oneStoreWords)
  }

  def getCorpusWordsRes: List[storeWordsRes] = {
    getURLs.map(x => {
        val (sid, url1) = (x._1, x._2)
        if (debugPrint) println("--------\nStore URL ---> " + url1)

        val fileName = localWorkDir + "store_" + sid + ".json"
        val json: String =
          if (saveToFile) {                                          //save data to a file
                val jsonFromWeb = getAPIData(url1)
                val w = new BufferedWriter(new FileWriter(fileName))
                w.write(jsonFromWeb); w.flush(); w.close()
                Source.fromFile(fileName).mkString
          }
          else if (runFromFile)  getFileData(fileName)               //run with data previously saved
          else getAPIData(url1)                                      //fetch data from web URL

      if (json.length < 20) {
          println("WARNING: " + sid + " API request did not get results")
          storeWordsRes(sid, Map(), Map())
        }
        else {
          val jValue = try {
              parse(json)
            }
            catch {
              case ex: Exception => println("WARNING 1: " + sid + " " + ex + ". continue...")
            }

          //get json object
          val parsedmap: Map[String, Any] = try {
            jValue.asInstanceOf[JObject].values
          }
          catch {
              case ex: Exception => println("WARNING 2: " + sid + " " + ex + ". continue...")
              Map()
          }

          //get results node from json
          val storeRes = try {
            parsedmap(rootJason).asInstanceOf[shopJsonType]
          }
          catch {
            case ex: Exception => println("WARNING 3: " + sid + " " + ex + ". continue...")
              List()
          }

          val oneStoreWords = getWordCounts(sid, storeRes, List())

          //add results for all listings
          if (oneStoreWords.size < 1)
            storeWordsRes(sid, Map(), Map())
          else {
            val tot_words: totWordsType = oneStoreWords.map(x => x.all_words).reduce((x, y) => addCounts(x, y))
            if (debugPrint) println("words with total: " + sid + ": " + tot_words)

            //get total words in the store
            val tot_store_words_count: Float = tot_words.toList.map(_._2).sum.toFloat
            if (debugPrint) println("tot_store_words_count: " + tot_store_words_count)

            //calculate term frequency tf
            val tf_words: tfWordsType = tot_words.toList.map(x => (x._1, x._2.toFloat / tot_store_words_count)).toMap //.asInstanceOf[tfWordsType]
            if (debugPrint) println("tf_words: " + sid + ": " + tf_words)

            //construct store level data
            storeWordsRes(sid, tot_words, tf_words)
          }
        }
      }
    )
  }

  def runShop: finalResType = {
    getFinalResults(getCorpusWordsRes)
  }
  //END
}
