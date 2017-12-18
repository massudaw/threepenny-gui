import Control.Monad (void)
import Data.Maybe
import Text.Printf
import Safe          (readMay)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup (return 1 ) (const (return ()))

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Test Speed"
    let msg = "This program tries to measure the speed at which HTML elements can be built."
    getBody #+ [UI.string msg, UI.br]
    UI.timestamp
    getBody #+ replicate 200 (UI.string "Haskell-")
    UI.timestamp
    getBody #+ [UI.string $ concat $ replicate 200 "Haskell-"]
    UI.timestamp
    getBody #+ [UI.string $ concat $ replicate 200 "Haskell-"]
    UI.timestamp
    getBody #+ [UI.string $ concat $ replicate 200 "Haskell-"]
    UI.timestamp
    getBody #+ [UI.string $ concat $ replicate 200 "Haskell-"]
    UI.timestamp
    getBody #+ [UI.string $ concat $ replicate 200 "Haskell-"]
    UI.timestamp
    getBody #+ [UI.string $ concat $ replicate 200 "Haskell-"]
    liftIOLater $ runUI window UI.timestamp

