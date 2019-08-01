{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Lib

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" $
  [ splitLineTests, scoreTests, extractNameTests, parseLineTests
  ]

splitLineTests = testGroup "splitLine"  $
    [
        testCase "Happy case" $  splitLine "Emotionality.............84 " @?= Right ("Emotionality", "84"),
        testCase "Incomplete" $  splitLine "a..." @?= Left (UnexpectedLine "a...")
    ]

scoreTests = testGroup "mkScore" $ 
    [
        testCase "Successful" $ mkScore 50 @?= Just (Score 50),
        testCase "Out of range" $ mkScore 101 @?= Nothing
    ]    

extractNameTests = testGroup "extractNameFromNameParagraph" $
    [
        testCase "Successful"   $ extractTextBetween "for: " ", who" "These results are for: Tin    , who"   @?= Just ("Tin"),
        testCase "Unsuccessful" $ extractTextBetween "about: " ", who" "These results are for: Tin    , who" @?= Nothing
    ]

parseLineTests = testGroup "parseLine" $ [
    testCase "Header" $ parseLine "Domain/Facet...... Score" @?= Right Header,
    testCase "Facet"  $ parseLine "Friendliness...... 51"    @?= Right (F "Friendliness" (Score 51)),
    testCase "Domain" $ parseLine "DOMAIN...... 51"          @?= Right (D "DOMAIN" (Score 51)),
    testCase "Domain spaces" $ parseLine "DOMAIN WITH SPACES...... 51"   @?= Right (D "DOMAIN WITH SPACES" (Score 51))
    ]    