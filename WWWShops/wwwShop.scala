package WWWShops
import scala.io.Source
import scala.math.log10

//base class
trait wwwShop {
  val shopName: String
  val shopNameShort: String
  val saveToFile: Boolean
  val runFromFile: Boolean
  //useless words to be removed from the text, can be improved
  //can also reside in the database
  val delete_words: List[String] = List("the","him","her","over","and","love","look","this","made",
    "from","with","size","for","use","measures","approximately","are",
    "inches","quot","long","because","such","some","may","not",
    "present","www","facebook","com","major","but","expected",
    "man","woman","material","please","note","appear","slightly",
    "different","you","can","also","find","women","men","that",
    "slight","print","color","colors","photos","finished","front")

  //temp work directory to be defined in application properties (database, reg, etc.)
  val localShopWorkDir = "C:\\Olya\\SimonData\\"

  //Basic types declarations to be used
  type totWordsType = Map[String, Int]
  type tfWordsType = Map[String, Float]
  type storeIdType = String
  type urlCollectionType = List[(String, String)]
  type finalResType = Seq[(String, List[wordProperties])]
  type shopJsonType                   //abstract shop json type to be defined in derived shop class

  case class storeWords(store_id: storeIdType, all_words: totWordsType)

  case class storeWordsRes(store_id: storeIdType,
                           tot_words: totWordsType,
                           tf_words: tfWordsType)

  //add two word counts maps together, recalculate the total counts for words
  def addCounts(words1: totWordsType, words2: totWordsType): totWordsType = {
    if (words1.isEmpty) words2
    else if (words2.isEmpty) words1
    else {
      val list = words1.toList ++ words2.toList
      list.groupMapReduce(x => x._1)(x => x._2)(_ + _)
    }
  }

  def getURLs: urlCollectionType          //get collection of URLs to visit
  def runShop: finalResType               //run the store implementation to get results

  def getWordCounts(sid:String,           //id of store within the web shop for tracking
                    jRes:shopJsonType,    //json type specific to the shop web api
                    oneStoreWords: List[storeWords]): List[storeWords]

  //case class to hold calculated word properties
  case class wordProperties (word: String, tf: Float, idf: Float, tf_idf: Float)

  //Returns number how many documents the word is in
  def getCount(word: String, corpusWordsRes: List[storeWordsRes]) : Int = {
    corpusWordsRes.count(_.tf_words.toList.map(_._1).contains(word))
  }

  var final_Res: finalResType = List()

  def getFinalResults(corpusWordsRes: List[storeWordsRes]):finalResType = {
    //Number of documents
    val N = corpusWordsRes.size

    //Final Results:
    //Idea is taken from
    //https://towardsdatascience.com/machine-learning-text-processing-1d5a2d638958
    //Store id followed by list of words with calculated values
    //Term Frequency (TF) = (Number of times term t appears in a document)/(Number of terms in the document)
    //Inverse Document Frequency (IDF) = log(N/n), where, N is the number of documents and n is the number of documents a term t has appeared in. The IDF of a rare word is high, whereas the IDF of a frequent word is likely to be low. Thus having the effect of highlighting words that are distinct.
    //We calculate TF-IDF value of a term as = TF * IDF
    final_Res =
    corpusWordsRes.map(x => {
        (x.store_id,
          x.tf_words.toList.map(y => {
            val word = y._1
            val word_tf = y._2                         //term frequency tf calculated earlier
            val n = getCount(word,corpusWordsRes)      //How many documents the word appears in
            val idf = log10(N/n).toFloat               //idf
            wordProperties(word, word_tf, idf, word_tf * idf)
          }).sortBy(_.tf_idf).reverse.take(5))  //sort by tf_idf in descending order and take 5 first
      }
    ).asInstanceOf[finalResType]
    final_Res
  }

  def getAPIData(url: String): String = {
    try {
      Source.fromURL(url).mkString                               //fetch data from web URL
    }
    catch {
      case ex: Exception => Console.println("WARNING API: " + ex + ". continue...")
        ""
    }
  }

  def getFileData(fileName: String): String = {
    try {
      Source.fromFile(fileName).mkString                         //fetch data from file
    }
    catch {
      case ex: Exception => println("WARNING API: " + ex + ". continue...")
        ""
    }
  }

  //overloaded to print results from object
  def printOutResults (): Unit = {
    printOutResults(final_Res)
  }

  //overloaded to print results any parameter of finalResType passed
  def printOutResults (final_Res: finalResType): Unit = {
    //if nothing to print
    if (final_Res.size < 1) {
      Console.println(shopName + " did not produce any reults")
      return
    }

    final_Res.foreach(x => {
      Console.println("\nStore: " + x._1)
      if(x._2.size < 1)
        Console.println("No Data found")
      else {
        Console.println("Word    \t\t\ttf\t\t    idf\t\t    tf-idf")
        x._2.foreach(y => {
          val printWord = y.word.padTo(20, ' ')
          Console.println("%s%.4f\t\t%.4f\t\t%.4f".format(printWord, y.tf, y.idf, y.tf_idf))
        })
      }
    })
  }  //printOutResults

  //END
}
