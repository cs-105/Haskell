import Test.HUnit

test1 = TestCase( assertEqual "Testing tests by running them a lot: " "pass" "pass" )

allTests :: Test
allTests = TestList [TestLabel "test1" test1]

main :: IO Counts
main = do _ <- runTestTT allTests
          runTestTT allTests'

