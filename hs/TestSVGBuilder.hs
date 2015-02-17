import Test.HUnit
import SVGBuilder

testJoinPathTuple :: Test
testJoinPathTuple = TestList [
    "123,456" ~=? joinPathTuple ("123","456")
    ]

testConvertRationalTupToString :: Test
testConvertRationalTupToString = TestList [
    
    ]

main :: IO ()
main = do
         runTestTT testJoinPathTuple
         return ()