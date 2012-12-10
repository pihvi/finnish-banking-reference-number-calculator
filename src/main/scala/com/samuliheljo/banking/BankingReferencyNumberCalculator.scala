package com.samuliheljo.banking

/**
 * Banking reference number calculator for finnish banks
 * @see http://www.fkl.fi/teemasivut/sepa/tekninen_dokumentaatio/Dokumentit/kotimaisen_viitteen_rakenneohje.pdf
 */
object BankingReferenceNumberCalculator {
  
    /**
     * Does calculation from given String and returns results as padded String
     */
    def calculate(from: String): String = {
        
        /* length is 3-19 numbers */
        var cleaned = from.replaceAll(" ","")
        require(cleaned.length <= 19)
        require(cleaned.length >= 3)
        
        var weights = List(3,7,1,3,7,1,3,7,1,3,7,1,3,7,1,3,7,1,3,7).drop(20-cleaned.length)
        var weightedPairs = weights.zip(cleaned.map(_.asDigit))        
        var weighted = weightedPairs.map(tuple => { tuple._1 * tuple._2})
        
        /* Weighted now contains reference digits multiplied with weight.
         * Calculate total sum and subtract that from next full tens to get checkdigit */
        var sum = weighted.reduce(_ + _)
        var lastDigit = sum.toString.takeRight(1).toInt
        var fullTens = sum + (10-lastDigit)
        var checkDigit = fullTens - sum
        if (checkDigit == 10) {
           checkDigit = 0
        }
        var referenceNumberWithCheckDigit = cleaned + checkDigit
        
        /* Now we have correct reference number. Group it to "fives"*/
        var grouped = referenceNumberWithCheckDigit.reverse.replaceAll("(.{5})", "$1 ").reverse   
        grouped.trim
    }
}
