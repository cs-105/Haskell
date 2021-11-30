import Test.HUnit

test1 = TestCase( assertEqual "If this test passes, Cabal has grabbed compatible packages and the project has built properly." "pass" "pass" )

allTests :: Test
allTests = TestList [TestLabel "Cabal build success" test1]

main = runTestTT allTests

