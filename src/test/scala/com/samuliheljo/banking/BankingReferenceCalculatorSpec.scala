package com.samuliheljo.banking

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.junit.Test

@RunWith(classOf[JUnitRunner])
class BankingReferenceNumberCalculatorSpecTest extends Specification {
    
    "BankingReferenceNumberCalculator" should {

        "calculate reference correctly" in {
            BankingReferenceNumberCalculator.calculate("123456") must equalTo("12 34561")
            BankingReferenceNumberCalculator.calculate("5213233021") must equalTo("5 21323 30214")
            BankingReferenceNumberCalculator.calculate("53040809") must equalTo("5304 08091")
            BankingReferenceNumberCalculator.calculate("11246393359") must equalTo("11 24639 33592")
            BankingReferenceNumberCalculator.calculate("21001650647506001") must equalTo("210 01650 64750 60018")
            BankingReferenceNumberCalculator.calculate("5345346325635435") must equalTo("53 45346 32563 54356")
            BankingReferenceNumberCalculator.calculate("5345346325635435230") must equalTo("53453 46325 63543 52305")
            BankingReferenceNumberCalculator.calculate("123743337137330") must equalTo("1 23743 33713 73300")
            BankingReferenceNumberCalculator.calculate("674") must equalTo("6745")
        }
        
        "fail when reference is shorter than 3 and longer than 19" in {
            BankingReferenceNumberCalculator.calculate("21") must throwA
            BankingReferenceNumberCalculator.calculate("12345678901234567890") must throwA
        }
        
        "Remove spaces correctly" in {
            BankingReferenceNumberCalculator.calculate("53 453 463 25 6354 3523   0") must equalTo("53453 46325 63543 52305")
        }
    }
}