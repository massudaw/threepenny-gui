{-# LANGUAGE QuasiQuotes #-}
module Foreign.JavaScript.Resources where

import           Data.Text                          (Text)
import qualified Data.Text                  as Text
import           Foreign.JavaScript.Include

jsDriverCode :: Maybe String -> Text
jsDriverCode dict = Text.unlines $ map Text.pack
    [ [include|js/lib/jquery.js|]
    , [include|js/lib/jquery-cookie.js|]
    , "var Haskell = {};" ++ maybe "" (\dict -> "dict = \""++ dict++ "\";") dict
    , [include|js/comm.js|]
    , [include|js/ffi.js|]
    , [include|js/lib.js|]
    , [include|js/log.js|]
    ]

cssDriverCode :: Text
cssDriverCode = Text.pack [include|js/haskell.css|]

defaultHtmlFile :: Text
defaultHtmlFile = Text.pack [include|js/index.html|]
