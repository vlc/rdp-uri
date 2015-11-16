{-# LANGUAGE OverloadedStrings #-}
module Main where

import RDPURI
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [units]
        where units =
                testCase "simple uri" $ "rdp://full%20address=s:mypc:3389&audiomode=i:2&disable%20themes=i:1" @=? renderRDPURI (RDPURI ("mypc", Just 3389) [AudioMode Two', DisableThemes One])
