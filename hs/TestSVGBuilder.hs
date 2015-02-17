import Test.HUnit
import SVGBuilder

testJoinPathTuple :: Test
testJoinPathTuple = TestList [
    "123,456" ~=? joinPathTuple ("123","456")
    ]

testConvertRationalTupToString :: Test
testConvertRationalTupToString = TestList [
    ("123.0","456.0") ~=? convertRationalTupToString (123, 456),
    ("123.123","456.456") ~=? convertRationalTupToString (123.123, 456.456)
    ]

testBuildPathString :: Test
testBuildPathString = TestList [
    "123.0,456.0" ~=? buildPathString [(123,456)],
    "123.0,456.0 123.123,456.456" ~=? buildPathString [(123, 456), (123.123, 456.456)]
    ]

main :: IO ()
main = do
         runTestTT testJoinPathTuple
         runTestTT testConvertRationalTupToString
         runTestTT testBuildPathString
         return ()