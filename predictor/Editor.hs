{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.List
import Control.Lens
import qualified Graphics.Vty as V
-- import CGram as G
import qualified Predictor as P

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core (str, vBox, hBox)
import MyEdit as E
import qualified Brick.AttrMap as A
import Brick.Widgets.Border as B
import Brick.Util()

data St = St {
    _edit :: E.Editor,
    _dict :: P.Dictionary
}

makeLenses ''St

drawUI :: St -> [T.Widget]
drawUI st = [vBox [E.renderEditor $ st^.edit,
                   B.hBorder, 
                   hBox [str ("Suggestions: " ++ joined)]]]
    where fixedWords = take 3 $ P.getMatchedWords (P.fixString $ E.lastWord (st^.edit)) (st^.dict)
          joined     = intercalate ", " fixedWords

appEvent :: St -> V.Event -> T.EventM (T.Next St)
appEvent st ev =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey (V.KChar '1') [V.MCtrl] -> M.continue st
        _ -> M.continue =<< T.handleEventLensed st edit ev

initialState :: St
initialState = St {
    _edit = E.editor "editor" (str . unlines) Nothing "",
    _dict = P.initialize
}

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []

appCursor :: St -> [T.CursorLocation] -> Maybe T.CursorLocation
appCursor _ = M.showCursorNamed "editor"

theApp :: M.App St V.Event
theApp =
    M.App {
        M.appDraw = drawUI,
        M.appChooseCursor = appCursor, 
        M.appHandleEvent = appEvent, 
        M.appStartEvent = return, 
        M.appAttrMap = const theMap, 
        M.appLiftVtyEvent = id
    }

main :: IO ()
main = do
    _ <- M.defaultMain theApp initialState
    putStrLn "done"