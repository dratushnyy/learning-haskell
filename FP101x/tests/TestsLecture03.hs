import Test.HUnit
import FP101x.Lecture03

testHalve = TestCase (assertEqual "halve [1,2,3,4,5,6]" (halve [1,2,3,4,5,6])  ([1,2,3], [4,5,6]) )
testsHalve = TestList [TestLabel "Test Halve" testHalve]

testAndTrueTrue = TestCase(assertEqual "True `and'` True " True (True `and'` True))
testAndTrueFalse = TestCase(assertEqual "True `and'` False " False (True `and'` False))
testAndFalseTrue = TestCase(assertEqual "False `and'` True " False (False `and'` True))
testAndFalseFalse = TestCase(assertEqual "False `and'` False " False (False `and'` False))
testsAnd = TestList [
  TestLabel "True`and'`True" testAndTrueTrue,
  TestLabel "True`and'`False" testAndTrueFalse,
  TestLabel "False`and'`True" testAndFalseTrue,
  TestLabel "False`and'`False" testAndTrueTrue]



testOrTrueTrue = TestCase(assertEqual "True `or'` True " True (True `or'` True))
testOrTrueFalse = TestCase(assertEqual "True `or'` False " True (True `or'` False))
testOrFalseTrue = TestCase(assertEqual "False `or'` True " True (False `or'` True))
testOrFalseFalse = TestCase(assertEqual "False `or'` False " False (False `or'` False))
testsOr = TestList [
  TestLabel "True`or'`True" testAndTrueTrue,
  TestLabel "True`or'`False" testAndTrueFalse,
  TestLabel "False`or'`True" testAndFalseTrue,
  TestLabel "False`or'`False" testAndTrueTrue]

testAnd2TrueTrue = TestCase(assertEqual "True `and2` True " True (True `and2` True))
testAnd2TrueFalse = TestCase(assertEqual "True `and2` False " False (True `and2` False))
testAnd2FalseTrue = TestCase(assertEqual "False `and2` True " False (False `and2` True))
testAnd2FalseFalse = TestCase(assertEqual "False `and2` False " False (False `and2` False))
testsAnd2 = TestList [
  TestLabel "True`and2`True" testAndTrueTrue,
  TestLabel "True`and2`False" testAndTrueFalse,
  TestLabel "False`and2`True" testAndFalseTrue,
  TestLabel "False`and2`False" testAndTrueTrue]


testLuhnDouble1 = TestCase (assertEqual "luhnDouble 3" 6 (luhnDouble 3))
testLuhnDouble2 = TestCase (assertEqual "luhnDouble 6" 3 (luhnDouble 6))
testsLuhnDouble = TestList [
  TestLabel "luhnDouble 3" testLuhnDouble1,
  TestLabel "luhnDouble 6" testLuhnDouble2]