module TablingTests where

import qualified Data.List as List
import Test.Tasty
import Test.Tasty.HUnit

{-
newtype HoleChar = HoleChar Char
  deriving (Eq, Ord)

instance HasHole HoleChar where
  hole = HoleChar 'X'

q :: String -> [HoleChar]
q = fmap HoleChar

tablingTests =
  testGroup
    "tabling"
    [ handlesQueriesWithHoles,
      handlesQueriesWithNoResults,
      emptyTablesResultInNoResults
    ]

emptyTablesResultInNoResults = testCase "empty tables result in no results" $ do
  answers (q "a") Dtsa.empty @?= ([] :: [Int])

handlesQueriesWithNoResults = testCase "handles queries with no results" $ do
  answers (q "xyz") tbl @?= []
  where
    tbl =
      List.foldr1 merge $
        [ build (q "aaa") 0,
          build (q "abc") 1
        ]

handlesQueriesWithHoles = testCase "handles queries with holes" $ do
  answers (q "aXc") tbl @?= [1]
  answers (q "Xbc") tbl @?= [1]
  answers (q "abX") tbl @?= [1]
  answers (q "aXX") tbl @?= [0, 1]
  answers (q "XXX") tbl @?= [0, 1]
  where
    tbl =
      List.foldr1 merge $
        [ build (q "aaa") 0,
          build (q "abc") 1
        ]
-}
