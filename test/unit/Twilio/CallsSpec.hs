{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Twilio.CallsSpec where

import           Test.Hspec

import           Control.Exception
import qualified Control.Monad.Trans.Free as Free
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import qualified Network.HTTP.Client      as HTTP
import qualified Network.HTTP.Types       as HTTP
import qualified Network.URI              as URI

import           Control.Monad.Twilio
import qualified Twilio.Calls             as Calls
import qualified Twilio.Internal.Request  as Request
import           Twilio.Types

spec :: Spec
spec = do
  describe "post Calls" $ do
    it "encodes all supported parameters" $ do
      let Just url1 = URI.parseAbsoluteURI "https://demo.twilio.com/welcome/voice/1"
      let Just url2 = URI.parseAbsoluteURI "https://demo.twilio.com/welcome/voice/2"
      let Just url3 = URI.parseAbsoluteURI "https://demo.twilio.com/welcome/voice/3"
      let request = unwrapFirstRequest $ Calls.post Calls.PostCalls
            { Calls.from                 = "+48000000000"
            , Calls.to                   = "+48000000001"
            , Calls.urlOrApplicationSID  = Left url1
            , Calls.method               = Just "POST"
            , Calls.fallbackURL          = Just url2
            , Calls.fallbackMethod       = Just "GET"
            , Calls.statusCallback       = Just url3
            , Calls.statusCallbackMethod = Just "POST"
            , Calls.sendDigits           = Just "234"
            , Calls.ifMachine            = Just Calls.Hangup
            , Calls.timeout              = Just 12
            , Calls.record               = Just True
            }
      body <- readRequestBody $ HTTP.requestBody request
      HTTP.parseSimpleQuery body `shouldBe`
        [ ("To","+48000000001")
        , ("From","+48000000000")
        , ("Url","https://demo.twilio.com/welcome/voice/1")
        , ("Method","POST")
        , ("FallbackUrl","https://demo.twilio.com/welcome/voice/2")
        , ("FallbackMethod","GET")
        , ("StatusCallback","https://demo.twilio.com/welcome/voice/3")
        , ("StatusCallbackMethod","POST")
        , ("SendDigits","234")
        , ("IfMachine","Hangup")
        , ("Timeout","12")
        , ("Record","true")
        ]

readRequestBody :: HTTP.RequestBody -> IO BS.ByteString
readRequestBody = \case
  HTTP.RequestBodyLBS lbs -> pure (LBS.toStrict lbs)
  HTTP.RequestBodyBS bs -> pure bs
  _ -> error "unsupported body"

unwrapFirstRequest :: TwilioT (Either SomeException) a -> HTTP.Request
unwrapFirstRequest (TwilioT f) =
  let accsid = mkAccountSID 0 0
      Just token = parseAuthToken ""
  in case f ((accsid, token), accsid) of
    Request.RequestT (Free.FreeT (Right (Free.Free (Request.RequestF (x, _))))) -> x
    _ -> error "unexpected result"
