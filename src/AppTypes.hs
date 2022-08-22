{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AppTypes where 

import Data.Default (Default(..))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Lens hiding ((<|), (|>), (:>), (:<))

import Data.Sequence (Seq(..))
import Data.Text (Text)
import Linear.V2 (V2(..), _x, _y)

-- -------------------------------------------------------------------
-- Transformer Stack

type MRSWIO a = MaybeT (ReaderT Config (StateT Game (WriterT Text IO))) a

-- -------------------------------------------------------------------
-- Environment

data Config = Config
  { _width            :: Int
  , _height           :: Int
  , _scoreIncGoodFood :: Int
  , _scoreIncBadFood  :: Int
  } deriving (Eq, Show)

instance Default Config where
  def = Config
    { _width            = 20
    , _height           = 20
    , _scoreIncGoodFood = 10
    , _scoreIncBadFood  = -30
    }

-- -------------------------------------------------------------------
-- State 

data Game = Game
  { _snake       :: Snake          -- ^ snake as a sequence of points in V2
  , _dir         :: Direction      -- ^ direction
  , _foodP       :: Coord          -- ^ location of the food
  , _foodM       :: Coord          -- ^ location of the double food
  , _dead        :: Bool           -- ^ game over flag
  , _paused      :: Bool           -- ^ paused flag
  , _score       :: Int            -- ^ score
  , _locked      :: Bool           -- ^ lock to disallow duplicate turns between time steps
  , _spawnFoodP  :: Bool
  , _spawnFoodM  :: Bool 
  } deriving (Show)

type Coord = V2 Int
type Snake = Seq Coord

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

-- -------------------------------------------------------------------
-- Logging 

data LogEvt = LogFoodPlus
            | LogFoodMinus
            | LogDead
            deriving (Eq, Show)

-- -------------------------------------------------------------------
-- Lenses

makeLenses ''Game
makeLenses ''Config

