module Main (main) where

import Test.HUnit

import MyLib

tests :: Test
tests = TestList
  [ "Test 1" ~: parseCSV ',' True "Name,Age,Gender\nJohn,25,Male\nJane,30,Female\n" ~?= Right (CSV ["Name", "Age", "Gender"] [["John", "25", "Male"], ["Jane", "30", "Female"]])
  , "Test 2" ~: parseCSV ';' True "Name;Age;Gender\nJohn;25;Male\nJane;30;Female\n" ~?= Right (CSV ["Name", "Age", "Gender"] [["John", "25", "Male"], ["Jane", "30", "Female"]])
  , "Test 3" ~: parseCSV ',' False "John,25,Male\nJane,30\n" ~?= Right (CSV [] [["John", "25", "Male"], ["Jane", "30"]])
  , "Test 4" ~: parseCSV ',' False "\n" ~?= Right (CSV [] [[""]])
  , "Test 5" ~: parseCSV ',' False "" ~?= Right (CSV [] [])
  ]

main :: IO ()
main = runTestTT tests >>= print
