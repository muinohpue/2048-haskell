{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import Happstack.Lite
import System.Random
import Data.Aeson (encode)
import Data.Text.Lazy (unpack)
import Control.Monad.Trans (liftIO)
import TwoZeroFourEight

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
    [ dir "hello" $ hello
    , dir "2048" $ dir "start" $ start
    , dir "2048" $ dir "move" $ move
    , hello
    ]

hello :: ServerPart Response
hello = ok $ toResponse ("Hello World!" :: String)

start :: ServerPart Response
start = do movements <- liftIO $ getStdRandom initMovements
           ok $ toResponse $ encode movements

move :: ServerPart Response
move = do method POST
          txtDrct <- lookText "direction"
          txtTable <- lookText "table"
          let drct = read $ unpack txtDrct :: Direction
              table = read $ unpack txtTable :: [[Int]]
              mss = getMovements drct table
          movements <- liftIO $ getStdRandom (setNewNumber mss)
          ok $ toResponse $ encode movements
