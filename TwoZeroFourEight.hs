{-# LANGUAGE OverloadedStrings #-}

module TwoZeroFourEight
( Direction(..)
, Position
, Movement
, getMovements
, setNewNumber
, initMovements
, fromMovements
) where

import Prelude hiding (Left, Right)
import Data.List
import System.Random
import Control.Applicative
import Data.Aeson

data Direction = Up | Down | Left | Right deriving (Read)

data Position = Position Int Int
                deriving (Show, Eq)

data Movement = Add Position Position Int | New Int | Keep Position Int | Empty
              deriving (Show, Eq)

instance ToJSON Movement where
    toJSON (Add (Position x1 y1) (Position x2 y2) n) =
        object [ ("type", toJSON ("add" :: String))
               , ("source", toJSON [ object [("x", toJSON x1), ("y", toJSON y1)]
                                   , object [("x", toJSON x2), ("y", toJSON y2)]])
               , ("value", toJSON n)
               ]
    toJSON (Keep (Position x y) n) =
        object [ ("type", toJSON ("keep" :: String))
               , ("source", object [("x", toJSON x), ("y", toJSON y)])
               , ("value", toJSON n)
               ]
    toJSON (New n) =
        object [ ("type", toJSON ("new" :: String))
               , ("value", toJSON n)
               ]
    toJSON Empty =
        object [ ("type", toJSON ("empty" :: String))
               , ("value", toJSON (0 :: Int))
               ]

locate :: Position -> [[a]] -> a
locate (Position x y) zss = zss !! x !! y

getMovements :: Direction -> [[Int]] -> [[Movement]]
getMovements drct nss =
    let xSize = length nss
        ySize = length $ head nss
        pss = foldr (\x pss ->
                      foldr (\y ps -> Position x y : ps) [] [0..(ySize-1)]
                      : pss)
                    [] [0..(xSize-1)]
        matchl [] = []
        matchl (p:[])
            | n == 0 = []
            | otherwise = Keep p n : []
          where n = locate p nss
        matchl lst@(p1:p2:ps)
            | n1 == 0 = matchl (p2:ps)
            | n2 == 0 = matchl (p1:ps)
            | n1 == n2 = Add p1 p2 (n1+n2) : matchl ps
            | otherwise = Keep p1 n1 : matchl (p2:ps)
          where n1 = locate p1 nss
                n2 = locate p2 nss
        calculate ps =
          let matched = matchl ps
          in matched ++ replicate ((length ps) - (length matched)) Empty
    in case drct of Left -> map calculate pss
                    Right -> turnBack $ map calculate $ turnBack pss
                    Down -> turnLeft $ map calculate $ turnRight pss
                    Up -> turnRight $ map calculate $ turnLeft pss

setNewNumber :: [[Movement]] -> StdGen -> ([[Movement]], StdGen)
setNewNumber mss sg =
    let xSize = length mss
        ySize = length $ head mss
        ps = filter (\(Position x y) -> Empty == mss !! x !! y) $
                 Position <$> [0..(xSize-1)] <*> [0..(ySize-1)]
        ns = [2, 2, 2, 4]
    in let (p, nsg) = choose ps sg
           (n, nnsg) = choose ns nsg
       in (setPosition p (New n) mss, nnsg)

initMovements :: StdGen -> ([[Movement]], StdGen)
initMovements sg =
    let (p, nsg) = choose (Position <$> [0..3] <*> [0..3]) sg
        mss = setPosition p (New 2) $ replicate 4 $ replicate 4 Empty
    in setNewNumber mss nsg

choose :: [a] -> StdGen -> (a, StdGen)
choose xs sg =
    let (i, nsg) = randomR (0, length xs - 1) sg
    in (xs !! i, nsg)

setPosition :: Position -> a -> [[a]] -> [[a]]
setPosition (Position x y) w zss =
    let (hs, (zs:rs)) = splitAt x zss
        (h, (z:r)) = splitAt y zs
    in hs ++ (h ++ w:r):rs

fromMovements :: [[Movement]] -> [[Int]]
fromMovements ass =
    map (map (\a -> case a of (Add _ _ n) -> n
                              (New n) -> n
                              (Keep _ n) -> n
                              Empty -> 0)) ass

turnLeft :: [[a]] -> [[a]]
turnLeft = reverse . transpose

turnRight :: [[a]] -> [[a]]
turnRight = transpose . reverse

turnBack :: [[a]] -> [[a]]
turnBack = reverse . (map reverse)
