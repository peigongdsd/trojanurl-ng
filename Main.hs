{-# LANGUAGE OverloadedStrings #-}
module Main where
import Include.Server ( runserver )

main :: IO ()
main = runserver "127.0.0.1" 2021

