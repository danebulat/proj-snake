{-# LANGUAGE OverloadedStrings #-}

module Main where 

import Test.Hspec
import Snake
import AppTypes

-- -------------------------------------------------------------------
-- Test functions

testOrthogonalTurn :: Bool
testOrthogonalTurn = turnDir South East  == South
                  && turnDir South West  == South
                  && turnDir North East  == North
                  && turnDir North West  == North
                  && turnDir East  North == East
                  && turnDir East  South == East
                  && turnDir West  North == West
                  && turnDir West  South == West 

testOrthogonalTurnOnly :: Bool
testOrthogonalTurnOnly = turnDir South North == North
                      && turnDir North South == South
                      && turnDir East  West  == West
                      && turnDir West  East  == East

-- -------------------------------------------------------------------
-- Main

main :: IO ()
main = hspec $ do
  
  describe "Turn Functionality" $ do
    it "Snake turns orthogonally" $ do
      testOrthogonalTurn

    it "Snake only turns orthogonally" $ do
      testOrthogonalTurnOnly
      

