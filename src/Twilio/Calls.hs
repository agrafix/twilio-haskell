{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Twilio.Calls
-- Copyright   :  (C) 2017- Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Twilio.Calls
  ( -- * Resource
    Calls(..)
  , Twilio.Calls.get
  , Twilio.Calls.post
  , PostCalls(..)
  , IfMachine(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Text (Text, pack)
import Data.Text.Encoding
import Network.URI

import Control.Monad.Twilio
import Twilio.Call
import Twilio.Internal.Request
import Twilio.Internal.Resource as Resource
import Twilio.Types

{- Resource -}

data Calls = Calls
  { callsPagingInformation :: PagingInformation
  , callList :: [Call]
  } deriving (Show, Eq)

instance List Calls Call where
  getListWrapper = wrap (Calls . fromJust)
  getList = callList
  getPlural = Const "calls"

instance FromJSON Calls where
  parseJSON = parseJSONToList

instance Get0 Calls where
  get0 = request parseJSONFromResponse =<< makeTwilioRequest
    "/Calls.json"

{- | Get 'Calls'.

For example, you can fetch the 'Calls' resource in the 'IO' monad as follows:

>module Main where
>
>import Control.Monad.IO.Class (liftIO)
>import System.Environment (getEnv)
>import Twilio.Calls as Calls
>import Twilio.Types
>
>-- | Print calls.
>main :: IO ()
>main = runTwilio' (getEnv "ACCOUNT_SID")
>                  (getEnv "AUTH_TOKEN")
>     $ Calls.get >>= liftIO . print
-}
get :: MonadThrow m => TwilioT m Calls
get = Resource.get

data PostCalls = PostCalls
  { from                 :: !Text
  , to                   :: !Text
  , urlOrApplicationSID  :: !(Either URI ApplicationSID)
  , method               :: !(Maybe Text)
  , fallbackURL          :: !(Maybe URI)
  , fallbackMethod       :: !(Maybe Text)
  , statusCallback       :: !(Maybe URI)
  , statusCallbackMethod :: !(Maybe Text)
  , sendDigits           :: !(Maybe Text)
  , ifMachine            :: !(Maybe IfMachine)
  , timeout              :: !(Maybe Integer)
  , record               :: !(Maybe Bool)
  } deriving (Show, Eq)

instance Post1 PostCalls Call where
  post1 postCalls
    = (request parseJSONFromResponse =<<)
    . makeTwilioPOSTRequest "/Calls.json"
    . catMaybes . map sequence $
      [ ("To", Just . encodeUtf8 $ Twilio.Calls.to postCalls)
      , ("From", Just . encodeUtf8 $ Twilio.Calls.from postCalls)
      , ("Url", encodeUrl <$> url)
      , ("Method", encodeUtf8 <$> method postCalls)
      , ("ApplicationSid", encodeUtf8 . pack . show <$> applicationSid)
      , ("FallbackUrl", encodeUrl <$> fallbackURL postCalls)
      , ("FallbackMethod", encodeUtf8 <$> fallbackMethod postCalls)
      , ("StatusCallback", encodeUrl <$> statusCallback postCalls)
      , ("StatusCallbackMethod", encodeUtf8 <$> statusCallbackMethod postCalls)
      , ("SendDigits", encodeUtf8 <$> sendDigits postCalls)
      , ("IfMachine", encodeUtf8 . pack . show <$> ifMachine postCalls)
      , ("Timeout", encodeUtf8 . pack . show <$> timeout postCalls)
      , ("Record", encodeBool <$> record postCalls)
      ]
    where
      encodeUrl :: URI -> ByteString
      encodeUrl = encodeUtf8 . pack . show

      encodeBool :: Bool -> ByteString
      encodeBool True = "true"
      encodeBool False = "false"

      url :: Maybe URI
      applicationSid :: Maybe ApplicationSID
      (url, applicationSid) =
        case urlOrApplicationSID postCalls of
          Left url -> (Just url, Nothing)
          Right applicationSid -> (Nothing, Just applicationSid)

instance Post3 Text Text URI Call where
  post3 to from url = request parseJSONFromResponse =<<
    makeTwilioPOSTRequest "/Calls.json"
      [ ("To", encodeUtf8 to)
      , ("From", encodeUtf8 from)
      , ("Url", encodeUtf8 . pack $ show url)
      ]

post :: MonadThrow m => PostCalls -> TwilioT m Call
post = Resource.post

data IfMachine
  = Continue
  | Hangup
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance FromJSON IfMachine where
  parseJSON (String "Continue") = return Continue
  parseJSON (String "Hangup")   = return Hangup
  parseJSON _ = mzero

instance ToJSON IfMachine where
  toJSON Continue = String "Continue"
  toJSON Hangup   = String "Hangup"
