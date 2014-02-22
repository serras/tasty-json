-- | Run a 'Tasty.TestTree' and produce a JSON file summarising the test results
-- in the same schema that would be produced by HTF --json test runner.
--
-- Based on the 'HTF' and 'tasty-ant-xml' packages.
{-# LANGUAGE OverloadedStrings #-}
module Test.Tasty.Runners.JsonTypes where

import qualified Data.Aeson as J
import Data.Aeson ((.=))

import Test.Tasty.Runners

class J.ToJSON a => TastyJsonObj a

-- "test-start" message
data TestStartEventObj
    = TestStartEventObj { ts_test :: TestObj }

instance J.ToJSON TestStartEventObj where
    toJSON ts =
        J.object ["type" .= J.String "test-start"
                 ,"test" .= J.toJSON (ts_test ts)]

instance TastyJsonObj TestStartEventObj

-- "test-end" message
data TestEndEventObj
    = TestEndEventObj
      { te_test       :: TestObj
      , te_result     :: Status
      , te_message    :: String
      , te_wallTimeMs :: Int
      }

instance J.ToJSON TestEndEventObj where
    toJSON te =
        J.object ["type"     .= J.String "test-end"
                 ,"test"     .= J.toJSON (te_test te)
                 ,"result"   .= J.toJSON (te_result te)
                 ,"message"  .= J.toJSON (te_message te)
                 ,"wallTime" .= J.toJSON (te_wallTimeMs te)]

instance TastyJsonObj TestEndEventObj

instance J.ToJSON Status where
    toJSON r = J.String $
        case r of
          Done (Result True  _) -> "pass"
          Done (Result False _) -> "fail"
          Exception _ -> "error"
          Executing _ -> "pending"
          NotStarted  -> "pending"

-- "test-list" message
data TestListObj
    = TestListObj
      { tlm_tests :: [TestObj]
      }

instance J.ToJSON TestListObj where
    toJSON tl =
        J.object ["type" .= J.String "test-list"
                 ,"tests" .= J.toJSON (tlm_tests tl)]

instance TastyJsonObj TestListObj

-- "test-results"
data TestResultsObj
    = TestResultsObj
      { tr_wallTimeMs :: Int
      , tr_passed :: Int
      , tr_pending :: Int
      , tr_failed :: Int
      , tr_errors :: Int
      }

instance J.ToJSON TestResultsObj where
    toJSON r = J.object ["type"     .= J.String "test-results"
                        ,"passed"   .= J.toJSON (tr_passed r)
                        ,"pending"  .= J.toJSON (tr_pending r)
                        ,"failures" .= J.toJSON (tr_failed r)
                        ,"errors"   .= J.toJSON (tr_errors r)
                        ,"wallTime" .= J.toJSON (tr_wallTimeMs r)]

instance TastyJsonObj TestResultsObj

data TestObj
    = TestObj
      { to_flatName :: String
      , to_path     :: [String]
      }

instance J.ToJSON TestObj where
    toJSON t = J.object (["flatName" .= J.toJSON (to_flatName t)
                         ,"path"     .= J.toJSON (to_path t)])

