{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Control.Lens ((^.), (.~), (&))
import Data.Maybe (fromMaybe)
import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text as T

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, padLeftRight, Padding(..)
  , withBorderStyle
  , txt, fill
  , viewport, viewportScroll, ViewportScroll, vScrollToEnd, vScrollBy
  , (<+>), (<=>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Types (ViewportType(..))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

import AppTypes
import Snake

-- -------------------------------------------------------------------
-- Types

-- | Ticks custom event 
data Tick = Tick

-- | Named resources
data Name = VP1 deriving (Ord, Show, Eq)

data Cell = Snake | Food | DoubleFood | Empty

vp1Scroll :: ViewportScroll Name
vp1Scroll = viewportScroll VP1

-- shortcut to append Text 
(<^>) :: Text -> Text -> Text
(<^>) = T.append 

-- -------------------------------------------------------------------
-- App definition

app :: App Game Tick Name
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000

  -- initial Game instance 
  g <- initGame def

  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty    -- Vty 
                    builder       -- IO Vty 
                    (Just chan)   -- Maybe (BChan e)
                    app           -- App s e n
                    g             -- s

-- -------------------------------------------------------------------
-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)

-- call step to continue the game 
handleEvent g (AppEvent Tick) = do

  -- perform step
  (g', w) <- liftIO (step g)

  -- scroll log viewport to end and add new data if new events occured 
  if null w && not (g' ^. updateLog)
    then continue g'
    else do vScrollToEnd vp1Scroll
            continue $ g' & logText .~ (g' ^. logText) ++ w
                          & updateLog .~ False

-- handle turning 
handleEvent g (VtyEvent (V.EvKey V.KUp    []))      = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown  []))      = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft  []))      = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = vScrollBy vp1Scroll (-1) >> continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = vScrollBy vp1Scroll 1 >> continue g

-- reset game 
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame def) >>= continue

-- quit game
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

-- -------------------------------------------------------------------
-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ vBox
    [ drawStats g
      <=> vBox [ C.center $  drawGrid g def]
      <=> drawLogger g
    ]
  ]

drawStats :: Game -> Widget Name
drawStats g =
  hBox [ withBorderStyle BS.unicode
       $ B.borderWithLabel (txt "| Snake |")
       $ padAll 1
       $ vLimit 1
       $ C.center
       $ sc
       <+> withAttr snakeAttr (txt "  ")
       <+> txt (" " <^> snakeLength g <^> "  ")
       <+> withAttr foodPAttr (txt "  ") <+> cp
       <+> withAttr foodMAttr (txt "  ") <+> cm
       ]
  where snakeLength g = T.pack . show $ 1 + (g ^. foodPCount) - (g ^. foodMCount)
        sc = txt ("Score " <^> (T.pack . show $ g  ^. score)       <^> "  ")
        cp = txt (" "      <^> (T.pack . show $ (g ^. foodPCount)) <^> "  ")
        cm = txt (" "      <^> (T.pack . show $ (g ^. foodMCount)) <^> "  ")

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.center $ txt "GAME OVER"
     else C.center $ txt "Playing..."

drawLogger :: Game -> Widget Name
drawLogger g = pair
  where
     pair = B.borderWithLabel (txt "| Logger |") $ vLimit 6 $ hBox
            [ viewport VP1 Vertical
            $ padLeftRight 1
            $ vBox (drawLog g)
            ]
            <+> vBox [ txt "\n", B.vBorder ]
            <+> drawUsage
     drawLog g = map (\x -> txt "\x03BB: " <+> (applyStyle $ fst x) (txt $ snd x)) logData
       where logData = g ^. logText
             applyStyle evt = case evt of
               LogFoodPlus  -> withAttr greenLogAttr
               LogFoodMinus -> withAttr redLogAttr
               LogDead      -> withAttr redLogAttr
               LogTurn      -> withAttr blueLogAttr

drawUsage = C.center
          $ txt "<arrows>   Move snake\n\
                \Esc, q     Quit\n\
                \r          Reset game\n\
                \k, j       Scroll logger"

drawGrid :: Game -> Config -> Widget Name
drawGrid g e = withBorderStyle BS.unicodeBold
  $ B.border
  $ vBox rows
  where
    h = e ^. height
    w = e ^. width
    rows         = [ hBox $ cellsInRow r | r <- [h-1, h-2 .. 0] ]
    cellsInRow y = [ drawCoord (V2 x y)  | x <- [0 .. w-1] ]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. snake = Snake
      | c ==     g ^. foodP = Food
      | c ==     g ^. foodM = DoubleFood
      | otherwise           = Empty

drawCell :: Cell -> Widget Name
drawCell Snake      = withAttr snakeAttr cellW
drawCell Food       = withAttr foodPAttr cellW
drawCell DoubleFood = withAttr foodMAttr cellW
drawCell Empty      = withAttr emptyAttr cellW

cellW :: Widget Name
cellW = txt "  "

-- -------------------------------------------------------------------
-- Attributes

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (snakeAttr, V.blue  `on` V.blue)
  , (foodPAttr, V.red   `on` V.red)
  , (foodMAttr, V.green `on` V.green)
  , (gameOverAttr,  fg V.red   `V.withStyle` V.bold)
  , (highlightAttr, fg V.brightWhite `V.withStyle` V.bold)

  -- logging styles
  , (redLogAttr,   V.brightRed   `on` V.black)
  , (greenLogAttr, V.brightGreen `on` V.black)
  , (blueLogAttr,  V.brightBlue  `on` V.black)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

snakeAttr, foodPAttr, foodMAttr, emptyAttr, highlightAttr :: AttrName
snakeAttr     = "snakeAttr"
foodPAttr     = "foodPAttr"
emptyAttr     = "emptyAttr"
foodMAttr     = "foodMAttr"
highlightAttr = "highlightAttr"

redLogAttr, greenLogAttr, blueLogAttr :: AttrName
redLogAttr   = "redLogAttr"
greenLogAttr = "greenLogAttr"
blueLogAttr  = "blueLogAttr"
