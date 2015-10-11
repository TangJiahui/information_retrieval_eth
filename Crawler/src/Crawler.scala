import org.jsoup.Jsoup;
import org.jsoup;
import scala.math.pow;
import java.util.Iterator;
import scala.collection.mutable.ListBuffer;
import scala.collection.mutable.Set;
import scala.collection.mutable.Map;
import java.security.MessageDigest;
import scala.util.Random;
import scala.util.control._;
import scala.util.hashing;
import scala.Long;

/**
 * @author yxchng
 */

object Crawler {
 
	def main(args: Array[String]) {
   
    
  
    val t0 = System.currentTimeMillis;
    
    
    def hash(s: String) : Long = {
      var h = 1125899906842597L; 
      val len = s.length();

      for (i <- 0 to len - 1) {
        h = 31*h + s.charAt(i);
      }
      return h;
    }
    
		def MD5(s: String) = {
			val m = java.security.MessageDigest.getInstance("MD5");
			val b = s.getBytes("UTF-8");
			m.update(b, 0, b.length);
			new java.math.BigInteger(1, m.digest()).toString(16);
		}

		def binary(value: Long) : String = {
			String.format("%64s", value.toBinaryString).replace(' ', '0')
		}
    
		def isNearDuplicate(fingerprint: String, database: Set[String]): Boolean = {
			for (pastFingerprint <- database) {
				var hammingDistance = 0;

				for (j <- 0 to pastFingerprint.length() - 1) {
					if (pastFingerprint(j) != fingerprint(j)) {
						hammingDistance = hammingDistance + 1;
          }
				}

				if (hammingDistance < 3) {
//          println(database);
//          println(s"Fingerprint 1: $fingerprint");
//          println(s"Fingerprint 2: $pastFingerprint");
//          println(hammingDistance)
          return true;
				}            
			}
      return false;
    }

		var URLFrontier = ListBuffer[String]();
		URLFrontier += "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/en.html";
    
		var crawled = Set[String](); //urls of crawled web-sites
    var crawledFingerprints = Set[String]();
		var uniqueSites = Set[Long](); //MD5 hashes of crawled web-sites
    
		var count = 0;
    var page = 0;
		var duplicatesCount = 0;
    var nearDupCount = 0;
    var studFreq = 0;

    while (!URLFrontier.isEmpty) {
//      print("page ")
//      println(page);
//      page = page + 1;
    	var currentFingerprint = "";

    	val url = URLFrontier.head;
    	val doc = Jsoup.connect(url).ignoreHttpErrors(true).get();
    	val docStr = doc.text().toLowerCase;
    	val wordFreq = docStr.split("\\W+").toList.groupBy((word: String) => word).mapValues(_.length);
    	studFreq = studFreq + wordFreq.getOrElse("student", 0);
    	val title = doc.select("title").text();

      //check if it is exact duplicate
			if (!title.contains("<title>404&nbsp;Page not found</title>") && title != "" && !title.contains("404 Not Found") && !title.contains("404")) {
				val docHash = hash(docStr);

				if (uniqueSites.contains(docHash)) {
          
//          println(url);
//          println(title);
//          println("xx")
					duplicatesCount = duplicatesCount + 1;
				} else {
					uniqueSites += docHash;    
          
          val html = doc.select("a[href]");
          val title = doc.select("title").text();

          //Convert textual data of web-site into binary string
          
          val textStr = doc.select("#contentMain").text();
          
          val tokens = textStr.split("[ .,;:?!\t\n\r\f]+").toList;
          val shingles = tokens.sliding(3).toSet;
          
          val shingleHashset = Set[Long]();
          
          for (shingle <- shingles) {
            val shingleStr = shingle.toString;
            val shingleHash = hash(shingleStr);
            shingleHashset += shingleHash;
          }
          
          
          
          val binaryStr = shingleHashset.map(h => binary(h));
          
          //Convert binary string to simHash
          val weights = Array.fill(64)(0);
          
          for (i <- binaryStr) {
        	  for (j <- 0 to 63) {
        		  if (i.charAt(j) == '1') {
        			  weights(j) = weights(j) + 1;
        		  } else {
        			  weights(j) = weights(j) - 1;
        		  }
        	  }  
          }

          for (j <- 0 to 63) {
        	  if (weights(j) >= 0) {
        		  currentFingerprint += "1";
        	  } else {
        		  currentFingerprint += "0";
        	  }
          }

          //check if it is near duplicate
          
          if (isNearDuplicate(currentFingerprint, crawledFingerprints)) {
            nearDupCount = nearDupCount + 1;
//            println(nearDupCount);
//            println(url);
//            println(textStr);
//            println(title);
//            println()
          }

          val links = html.listIterator();
          while (links.hasNext()) {
        	  val link = links.next();
        	  val relHref = link.attr("href");
        	  val absHref = link.attr("abs:href");
        	  val absHrefStr = absHref.toString;
            
        	  if (absHrefStr.startsWith("http://idvm-infk-hofmann03.inf.ethz.ch/") && absHrefStr.endsWith(".html")) {
              
        		  if (!absHrefStr.contains("login"))
        			  if (!URLFrontier.contains(absHrefStr)) {
        				  if (!crawled.contains(absHrefStr)) {
        					  URLFrontier += absHrefStr;
        					  count = count + 1;
        				  }
        			  
        		  }
        	  }
          }
          crawledFingerprints += currentFingerprint;
				}
//				if (currentFingerprint != "") {
//					crawledFingerprints += currentFingerprint;
//
//				}
			}
			crawled += URLFrontier.remove(0);
//      println(currentFingerprint);
//      println(url)
//      println("--")


    }

		println(s"Distinct URLs found:  $count");
		println(s"Exact duplicates found: $duplicatesCount");
    println(s"""Term frequency of "student": $studFreq""");
    println(s"Near duplicates found: $nearDupCount")
    val t1 = System.currentTimeMillis;
    val elapsedTime = (t1 - t0) / 60000.0;
    
    println(s"Time used: $elapsedTime minutes");
  }
//   def main(args: Array[String]) {
//     val doc = Jsoup.connect("http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/loginc5d9.html?resource=%2Fcontent%2Fmain%2Fen%2Fcampus%2Fbibliotheken-sammlungen-archive%2Fsammlungen-und-archive%2Fhaustier-anatomische-sammlung.html").ignoreHttpErrors(true).get();
//     val text = doc.select("#contentMain")
//     val title = doc.select("title").text();
//     println(title);
//     if (title == "Shibboleth Authentication Request") {
//       println("wooo");
//     }
//     val textStr = text.text();
//     if (textStr == "") {
//       println("yes");
//     }
//     
//     println(textStr)
//  }
    
  
}