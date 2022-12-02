{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main ( main ) where

import {-@ HTF_TESTS @-} TestZquot
import {-@ HTF_TESTS @-} TestZquotUtil
import {-@ HTF_TESTS @-} TestECGroup
import {-@ HTF_TESTS @-} TestECGroupUtil
import Test.Framework

main :: IO ()
main = htfMain htf_importedTests 
