import Test.HUnit

test1 = TestCase( assertEqual "Testing tests by running them a lot: " "pass" "pass" )

main = runTestTT test1

