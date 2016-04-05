{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Lens
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core (str)
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import Brick.Util()

data St = St {
    _currentEditor :: T.Name,
    _editor :: E.Editor
}

makeLenses ''St

drawUI :: St -> [T.Widget]
drawUI st = [E.renderEditor $ st^.editor]

appEvent :: St -> V.Event -> T.EventM (T.Next St)
appEvent st ev =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        _ -> M.continue =<< T.handleEventLensed st editor ev

initialState :: St
initialState = St "editor" (E.editor "editor" (str . unlines) Nothing "")

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []

appCursor :: St -> [T.CursorLocation] -> Maybe T.CursorLocation
appCursor st = M.showCursorNamed (st^.currentEditor)

theApp :: M.App St V.Event
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          , M.appLiftVtyEvent = id
          }

main :: IO ()
main = do
    _ <- M.defaultMain theApp initialState
    putStrLn "done"