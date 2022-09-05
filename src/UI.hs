{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)
import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text as T

import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Types (ViewportType(..))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Lens.Micro
import Lens.Micro.Mtl ((.=), (%=), use)

import AppTypes
import Snake
import Graphics.Vty (Event(EvKey))

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

app :: App AppState Tick Name
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return ()
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
                    (AppState g)  -- s

-- -------------------------------------------------------------------
-- Handling events

handleEvent :: BrickEvent Name Tick -> EventM Name AppState ()
handleEvent e =
  case e of
    AppEvent Tick -> do
      g <- use game
      
      -- perform step
      (g', w) <- liftIO (step g)

      if null w && not (g' ^. updateLog)
        then game .= g'
        else do 
          vScrollToEnd vp1Scroll
          --game .= g'
          game %= \_ -> g' & logText .~ (g' ^. logText) ++ w
                           & updateLog .~ False

    -- turn snake
    VtyEvent (V.EvKey V.KUp [])    -> game %= turn North
    VtyEvent (V.EvKey V.KDown [])  -> game %= turn South
    VtyEvent (V.EvKey V.KRight []) -> game %= turn East
    VtyEvent (V.EvKey V.KLeft [])  -> game %= turn West

    -- scroll viewport
    VtyEvent (V.EvKey (V.KChar 'k') []) -> vScrollBy vp1Scroll (-1)
    VtyEvent (V.EvKey (V.KChar 'j') []) -> vScrollBy vp1Scroll 1

    -- reset
    VtyEvent (V.EvKey (V.KChar 'r') []) -> do
      g <- liftIO $ initGame def
      game .= g

    -- exit
    VtyEvent (V.EvKey (V.KChar 'q') [])    -> halt
    VtyEvent (V.EvKey V.KEsc []) -> halt

    _ -> return ()

-- -------------------------------------------------------------------
-- Drawing

drawUI :: AppState -> [Widget Name]
drawUI st =
  [ vBox
    [ drawStats g
      <=> vBox [ C.center $  drawGrid g def]
      <=> drawLogger g
    ]
  ]
  where g = st^.game

drawStats :: Game -> Widget Name
drawStats g =
  hBox [ withBorderStyle BS.unicode
       $ B.borderWithLabel (txt "| Snake |")
       $ padAll 1
       $ vLimit 1
       $ C.center
       $ sc
       <+> withAttr (attrName "snakeAttr") (txt "  ")
       <+> txt (" " <^> snakeLength g <^> "  ")
       <+> withAttr (attrName "foodPAttr") (txt "  ") <+> cp
       <+> withAttr (attrName "foodMAttr") (txt "  ") <+> cm
       ]
  where snakeLength g = T.pack . show $ S.length (g ^. snake)
        sc = txt ("Score " <^> (T.pack . show $ g  ^. score)       <^> "  ")
        cp = txt (" "      <^> (T.pack . show $ (g ^. foodPCount)) <^> "  ")
        cm = txt (" "      <^> (T.pack . show $ (g ^. foodMCount)) <^> "  ")

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr (attrName "gameOver") $ C.center $ txt "GAME OVER"
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
               LogFoodPlus  -> withDefAttr (attrName "greenLogAttr")
               LogFoodMinus -> withDefAttr (attrName "redLogAttr")
               LogDead      -> withDefAttr (attrName "redLogAttr")
               LogTurn      -> withDefAttr (attrName "blueLogAttr")

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
drawCell Snake      = withDefAttr (attrName "snakeAttr") cellW
drawCell Food       = withDefAttr (attrName "foodPAttr") cellW
drawCell DoubleFood = withAttr (attrName "foodMAttr") cellW
drawCell Empty      = cellW

cellW :: Widget Name
cellW = txt "  "

-- -------------------------------------------------------------------
-- Attributes

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (attrName "snakeAttr",     V.blue  `on` V.blue)
  , (attrName "foodPAttr",     V.green `on` V.green)
  , (attrName "foodMAttr",     V.red   `on` V.red)
  , (attrName "gameOver",      fg V.red `V.withStyle` V.bold)
  , (attrName "highlightAttr", fg V.brightWhite `V.withStyle` V.bold)

  -- logging styles
  , (attrName "redLogAttr",   V.brightRed   `on` V.black)
  , (attrName "greenLogAttr", V.brightGreen `on` V.black)
  , (attrName "blueLogAttr",  V.brightBlue  `on` V.black)
  ]
