{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Snake
  ( initGame
  , step'
  , turn
  , Game(..)
  , Direction(..)
  , dead, food, score, snake, doubleFood
  , height, width
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Bool (bool)
import Data.Default
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen, randomRIO)

-- Types

-- Pass via ReaderT
data Env = Env
  { width'            :: Int
  , height'           :: Int
  , scoreIncGoodFood' :: Int
  , scoreIncBadFood'  :: Int
  } deriving (Eq, Show)

instance Default Env where
  def = Env
    { width'            = 20
    , height'           = 20
    , scoreIncGoodFood' = 10
    , scoreIncBadFood'  = -30
    }

data Game = Game
  { _snake       :: Snake          -- ^ snake as a sequence of points in V2
  , _dir         :: Direction      -- ^ direction
  , _food        :: Coord          -- ^ location of the food
  , _doubleFood  :: Coord          -- ^ location of the double food
  , _foods       :: Stream Coord   -- ^ infinite list of random next food locations
  , _dead        :: Bool           -- ^ game over flag
  , _paused      :: Bool           -- ^ paused flag
  , _score       :: Int            -- ^ score
  , _locked      :: Bool           -- ^ lock to disallow duplicate turns between time steps
  } deriving (Show)

type Coord = V2 Int

type Snake = Seq Coord

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game

-- Constants

height, width :: Int
height = 20
width = 20

-- Functions
-- | Top function for performing a game step 
step' :: Game -> IO Game
step' = execStateT (runMaybeT doStep)

-- | Entry point to the monad transformer stack 
doStep :: MaybeT (StateT Game IO) ()
doStep = do
  -- get game environment
  g <- lift get

  -- make sure the game isn't paused or over 
  let isPaused = g ^. paused
      isDead   = g ^. dead
      stopComp = isPaused || isDead
  MaybeT $ guard <$> (pure $ not stopComp)

  -- unlock from last directional turn
  lift $ modify (\s -> s & locked .~ False)

  die'                -- die (move into boundary), 
  eatFood'            -- eat (moved into food),
  eatDoubleFood
  lift $ modify move' -- move (move into space)

-- | Handle the possibility of game over 
die' :: MaybeT (StateT Game IO) ()
die' = do
  -- if next head is an element in Snake, set dead to True
  lift $ modify (\s -> let next = nextHead s in
                    if next `elem` s ^. snake
                      then s & dead .~ True else s)

-- | Move snake along in a marquee fashion
move' :: Game -> Game
move' g@Game { _snake = (s :|> _) } = g & snake .~ (nextHead g <| s)
move' _                             = error "Snakes can't be empty!"

-- | Handle snake head landing on food 
eatFood' :: MaybeT (StateT Game IO) ()
eatFood' = do
  g <- lift get
  if nextHead g == g ^. food
    then do
      lift $ modify (\s -> s & score +~ 10
                             & snake .~ nextHead s <| s ^. snake)
      nextFood'
    else MaybeT $ pure (Just ())

-- | Calculate a new food coordinate 
nextFood' :: MaybeT (StateT Game IO) ()
nextFood' = do
  -- TODO: Get width + height from Reader 
  -- generate new coordinates for next food
  x <- liftIO $ randomRIO (0, width - 1)
  y <- liftIO $ randomRIO (0, height - 1)

  -- check if it isn't on top of the snake
  s <- lift get
  if V2 x y `elem` s ^. snake || V2 x y == s ^. doubleFood
    then nextFood'
    else lift $ modify (\s' -> s' & food .~ V2 x y)

-- -------------------------------------------------------------------
eatDoubleFood :: MaybeT (StateT Game IO) ()
eatDoubleFood = do
  s <- lift get
  if nextHead s == s ^. doubleFood
    -- increase score and make snake shorter 
    then MaybeT $ fmap Just $ do
           modify $ \s ->
             let cs  = s ^. snake
                 cs' = if length cs <= 1 then cs else initSnake cs
             in s & score +~ (-30) & snake .~ cs'

           -- calculate next double food Coord 
           nextDoubleFood
      else MaybeT $ pure (Just ())
  where
    initSnake cs = S.deleteAt (S.length cs - 1) cs

nextDoubleFood :: StateT Game IO ()
nextDoubleFood = do
  x <- liftIO $ randomRIO (0, width - 1)
  y <- liftIO $ randomRIO (0, height - 1)

  -- check if new coord isn't on top of the snake
  s <- get
  if V2 x y `elem` s ^. snake || V2 x y == s ^. food 
    then nextDoubleFood
    else  modify (\s' -> s' & doubleFood .~ V2 x y)

-------------------------------------------------------------------

-- | Get next head position of the snake (called in `move` and `eatFoot`)
nextHead :: Game -> Coord
nextHead Game { _dir = d, _snake = (a :<| _) }            -- d :: Direction, a :: Coord
--                                  ^^^^^^^ head Coord of Snake 
  | d == North = a & _y %~ (\y -> (y + 1) `mod` height)   -- up one 
  | d == South = a & _y %~ (\y -> (y - 1) `mod` height)   -- down one
  | d == East  = a & _x %~ (\x -> (x + 1) `mod` width)    -- left one 
  | d == West  = a & _x %~ (\x -> (x - 1) `mod` width)    -- right one
  --                 ^^ modify _x or _y of the snake head Coord 
nextHead _ = error "Snakes can't be empty!"


-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet locks game
turn :: Direction -> Game -> Game
turn d g = if g ^. locked
  then g  -- return Game if game is locked (do not modify state)
  else g & dir %~ turnDir d & paused .~ False & locked .~ True

turnDir :: Direction -> Direction -> Direction
turnDir n c | c `elem` [North, South] && n `elem` [East, West] = n
              -- turn North or South if current dir is East of West 
            | c `elem` [East, West] && n `elem` [North, South] = n
            -- turn East or West if current dir is Noth or South
            | otherwise = c
            -- no change in direction

-- | Initialize a paused game with random food location

-- TODO: Functions for generating food x and y coords
-- TODO: Remove Stream functions
initGame :: IO Game
initGame = do
  (f :| (h :| fs)) <-
    fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        { _snake      = S.singleton (V2 xm ym)
        , _food       = f
        , _doubleFood = h
        , _foods      = fs
        , _score      = 0
        , _dir        = North
        , _dead       = False
        , _paused     = True
        , _locked     = False
        }

  r <- runStateT (runMaybeT nextFood') g
  return $ snd r 

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
