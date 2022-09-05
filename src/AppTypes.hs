{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AppTypes where 

import Data.Default (Default(..))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Lens.TH (makeLenses)

import Data.Sequence (Seq(..))
import Data.Text (Text)
import Linear.V2 (V2(..), _x, _y)

-- -------------------------------------------------------------------
-- Transformer Stack

type LogData = [(LogEvt, Text)]

type AppM a = MaybeT (ReaderT Config (StateT Game (WriterT (LogData) IO))) a

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

newtype AppState = AppState
  { _game :: Game }

data Game = Game
  { _snake       :: Snake
  , _dir         :: Direction
  , _foodP       :: Coord
  , _foodM       :: Coord
  , _dead        :: Bool   -- ^ game over flag
  , _paused      :: Bool   -- ^ paused flag
  , _score       :: Int    -- ^ score
  , _locked      :: Bool   -- ^ lock to disallow duplicate turns between time steps
  , _spawnFoodP  :: Bool
  , _spawnFoodM  :: Bool
  , _makeLonger  :: Bool
  , _logText     :: [(LogEvt, Text)]
  , _updateLog   :: Bool
  , _foodPCount  :: Int
  , _foodMCount  :: Int
  }

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
            | LogTurn 
            deriving (Eq, Show)

-- -------------------------------------------------------------------
-- Lenses

makeLenses ''Game
makeLenses ''Config
makeLenses ''AppState
