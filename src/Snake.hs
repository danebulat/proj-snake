{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Snake
  ( initGame
  , step
  , turn
  , Config (..) , Direction(..), Game(..)
  , dead, foodP, foodM, score, snake
  , height, width
  ) where

import Control.Monad (guard)
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (randomRIO)

import AppTypes

-- | Logging outputs 
logt :: LogEvt -> Text
logt LogFoodPlus  = "Ate food (+10 points)"
logt LogFoodMinus = "Ate food (-30 points)"
logt LogDead      = "Game over"
logt LogTurn      = "Turned "

-- | Top function for performing a game step 
step :: Game -> IO (Game, LogData)
step g = do
  (a, w) <- runWriterT (runStateT (runReaderT (runMaybeT doStep) def) g)
  return (snd a, w)

-- | Entry point to the monad transformer stack 
doStep :: AppM ()
doStep = do
  g <- lift $ lift get

  -- make sure the game isn't paused or over 
  let isPaused = g ^. paused
      isDead   = g ^. dead
      stopComp = isPaused || isDead
  MaybeT $ guard <$> (pure $ not stopComp)

  -- unlock from last directional turn
  lift $ lift $ modify (\s -> s & locked .~ False)

  -- handle game events (order or computation matters)
  eatPlus >> eatMinus >> move >> die >> mkFood

-- | Handle the possibility of game over 
die :: AppM ()
die = do
  -- dead if the snake's head is over another snake coord
  s <- lift $ lift get

  let (h :<| hs) = s ^. snake in
    if h `elem` hs
       then do lift $ lift $ modify (\s -> s & dead .~ True)
               lift $ lift $ lift $ tell [(LogDead, logt LogDead)]
       else MaybeT $ pure (Just ())

-- | Move snake along in a marquee fashion
move :: AppM ()
move = do
  n <- nextHead
  g <- lift $ lift get

  -- keep snake length the same if it's marked to be made longer 
  let cs = if g ^. makeLonger 
           then n <| g ^. snake
           else n <| S.deleteAt (truncLen g) (g ^. snake)

  -- update snake coords in state
  lift $ lift $ put (if g ^. makeLonger
                     then g & snake .~ cs & makeLonger .~ False
                     else g & snake .~ cs)
  where truncLen g = S.length (g ^. snake) - 1

-- | Handle generating new food coordinates
mkFood :: AppM ()
mkFood = do
  s <- lift $ lift get
  if s ^. spawnFoodP then mkFoodP else MaybeT $ pure (Just ())
  if s ^. spawnFoodM then mkFoodM else MaybeT $ pure (Just ())

-- | Handle snake head landing on food 
eatPlus :: AppM ()
eatPlus = do
  g <- lift $ lift get
  let (h :<| _) = g ^. snake 
  if h == g ^. foodP
    then do
      lift $ lift $ modify (\s -> s & score +~ 10
                                    & foodPCount +~ 1
                                    & makeLonger .~ True
                                    & spawnFoodP .~ True)
      lift $ lift $ lift $ tell [(LogFoodPlus, logt LogFoodPlus)]
    else MaybeT $ pure (Just ())

-- | Calculate a new food coordinate 
mkFoodP :: AppM ()
mkFoodP = do
  e <- lift ask
  s <- lift $ lift get
  x <- liftIO $ randomRIO (0, e ^. width - 1)
  y <- liftIO $ randomRIO (0, e ^. height - 1)

  -- check if it isn't on top of the snake
  if (V2 x y `elem` s ^. snake) || (V2 x y == s ^. foodM)
    then mkFoodP
    else lift $ lift $ modify (\s' -> s' & foodP .~ V2 x y
                                         & spawnFoodP .~ False)

-- -------------------------------------------------------------------
eatMinus :: AppM ()
eatMinus = do
  g <- lift $ lift get
  nxt <- nextHead
  if nxt == g ^. foodM
    then do
      lift $ lift $ modify (\s -> s & score +~ (-30)
                                    & snake .~ initSnake (s ^. snake)
                                    & spawnFoodM .~ True
                                    & (if snakeLen g /= 1
                                       then foodMCount +~ 1
                                       else foodMCount .~ g ^. foodMCount))
        
      lift $ lift $ lift $ tell [(LogFoodMinus, logt LogFoodMinus)]
    else MaybeT $ pure (Just ())
  where
    snakeLen g = S.length $ g ^. snake
    initSnake cs =
      if S.length cs <= 1 then cs else S.deleteAt (S.length cs - 1) cs

mkFoodM :: AppM ()
mkFoodM = do
  s <- lift $ lift get
  e <- lift ask
  x <- liftIO $ randomRIO (0, e ^. width - 1)
  y <- liftIO $ randomRIO (0, e ^. height - 1)

  -- check if new coord isn't on top of the snake
  if (V2 x y `elem` s ^. snake) || (V2 x y == s ^. foodP)
    then mkFoodM
    else lift $ lift $ modify (\s' -> s' & foodM .~ V2 x y
                                         & spawnFoodM .~ False)
-- -----------------------------------------------------------------

-- | Get next head position of the snake (called in move, eat* functions)
nextHead :: AppM (V2 Int)
nextHead = do
  r <- lift ask
  s <- lift $ lift get
  let (a :<| _) = s ^. snake

  case s ^. dir of
    North -> MaybeT $ pure $ Just $ a & _y %~ (\y -> (y + 1) `mod` (r ^. height))
    South -> MaybeT $ pure $ Just $ a & _y %~ (\y -> (y - 1) `mod` (r ^. height))
    East  -> MaybeT $ pure $ Just $ a & _x %~ (\x -> (x + 1) `mod` (r ^. width))
    West  -> MaybeT $ pure $ Just $ a & _x %~ (\x -> (x - 1) `mod` (r ^. width))

-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet locks game
turn :: Direction -> Game -> Game
turn d g = if g ^. locked
  then g  -- return Game if game is locked (do not modify state)
  else g & dir       %~ turnDir d
         & paused    .~ False
         & locked    .~ True
         & updateLog .~ turned
         & logText   .~ if turned then (g ^. logText) ++ mkLog d else g ^. logText
  where
    cur     = g ^. dir
    nxt     = turnDir d cur
    turned  = cur /= nxt
    mkLog d = [(LogTurn, logt LogTurn `T.append` T.pack (show d))]

-- | Return a possible new direction (rakes [n]ext and [c]urrent)
turnDir :: Direction -> Direction -> Direction
turnDir n c | c `elem` [North, South] && n `elem` [East, West] = n
            | c `elem` [East, West] && n `elem` [North, South] = n
            | otherwise = c

-- | Generate a random grid coordinate
--
-- Pass a list of coordinates to avoid those value pairs
randGridCoord :: [V2 Int] -> ReaderT Config IO (V2 Int)
randGridCoord cs = do
  r <- ask
  xy <- liftIO $ randXY r
  if xy `elem` cs then randGridCoord cs
                  else return xy
  where randXY r = do
          x <- randomRIO (0, r ^. width - 1)
          y <- randomRIO (0, r ^. height - 1)
          return $ V2 x y

-- | initialize a paused game with random food locations
initGame :: Config -> IO Game
initGame r = do
  let xm = r ^. width  `div` 2
      ym = r ^. height `div` 2
      z  = V2 xm ym -- snake start position

  f <- runReaderT (randGridCoord [z]) def
  h <- runReaderT (randGridCoord [z, f]) def

  let g  = Game
        { _snake      = S.singleton (V2 xm ym)
        , _foodP      = f
        , _foodM      = h
        , _score      = 0
        , _dir        = North
        , _dead       = False
        , _paused     = True
        , _locked     = False
        , _spawnFoodP = False
        , _spawnFoodM = False
        , _makeLonger = False
        , _logText    = []
        , _updateLog  = False
        , _foodPCount = 0
        , _foodMCount = 0
        }

  return g

