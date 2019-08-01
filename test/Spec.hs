import Test.Tasty
import Test.Tasty.HUnit

import Lib

import qualified Data.Text.IO as TIO

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" $
  [ splitLineTests, scoreTests, extractNameTests, parseLineTests, readWholeHtml
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

readWholeHtml = testCase "Whole Html" $ do 
    str <- TIO.readFile "results.html"
    let (Right result) = textToPersonalityTestResult "dummy@test.com" str
    result @?= PersonalityTestResult {
        _name = "Tin",
        _email = "dummy@test.com",
        _domains = [
            Domain "EXTRAVERSION" (Score 72) [
                    Facet "Friendliness" (Score 82),
                    Facet "Gregariousness" (Score 64),
                    Facet "Assertiveness" (Score 44),
                    Facet "Activity Level" (Score 70),
                    Facet "Excitement-Seeking" (Score 56),
                    Facet "Cheerfulness" (Score 76)
                ],
            Domain "AGREEABLENESS" (Score 82) [
                    Facet "Trust" (Score 42),
                    Facet "Morality" (Score 89),
                    Facet "Altruism" (Score 57),
                    Facet "Cooperation" (Score 54),
                    Facet "Modesty" (Score 74),
                    Facet "Sympathy" (Score 99)
                ],
            Domain "CONSCIENTIOUSNESS" (Score 48) [
                    Facet "Self-Efficacy" (Score 90),
                    Facet "Orderliness" (Score 7),   
                    Facet "Dutifulness" (Score 66),   
                    Facet "Achievement-Striving" (Score 84),   
                    Facet "Self-Discipline" (Score 36),   
                    Facet "Cautiousness" (Score 38)  
                ],
                Domain "NEUROTICISM" (Score 50) [
                    Facet "Anxiety" (Score 73),
                    Facet "Anger" (Score 27),   
                    Facet "Depression" (Score 30),   
                    Facet "Self-Consciousness" (Score 39),   
                    Facet "Immoderation" (Score 77),   
                    Facet "Vulnerability" (Score 58)  
                ],
                Domain "OPENNESS TO EXPERIENCE" (Score 80) [
                    Facet "Imagination" (Score 28),
                    Facet "Artistic Interests" (Score 40),   
                    Facet "Emotionality" (Score 84),   
                    Facet "Adventurousness" (Score 97),   
                    Facet "Intellect" (Score 48),   
                    Facet "Liberalism" (Score 93)  
                ]
            ] 
    }
