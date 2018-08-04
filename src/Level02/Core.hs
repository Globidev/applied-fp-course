{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
module Level02.Core (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           Level02.Types           (ContentType(..), Error(..),
                                           RqType(..),
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- --------------------------------------------
-- - Don't start here, go to Level02.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse ::
  Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status contentType lbs =
  let headers = [(hContentType, renderContentType contentType)]
  in responseLBS status headers lbs

resp200 ::
  ContentType
  -> LBS.ByteString
  -> Response
resp200 =
  mkResponse status200

resp404 ::
  ContentType
  -> LBS.ByteString
  -> Response
resp404 =
  mkResponse status404

resp400 ::
  ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  mkResponse status400

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest ::
  Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest topicText commentLBS =
  AddRq
    <$> mkTopic topicText
    <*> (mkCommentText . lazyByteStringToStrictText) commentLBS
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest ::
  Text
  -> Either Error RqType
mkViewRequest topicText =
  ViewRq <$> mkTopic topicText

mkListRequest ::
  Either Error RqType
mkListRequest =
  Right ListRq

mkErrorResponse ::
  Error
  -> Response
mkErrorResponse = \case
  EmptyTopic   -> resp400 PlainText "Topic cannot be empty"
  EmptyComment -> resp400 PlainText "Comment cannot be empty"
  InvalidPath  -> resp404 PlainText "Not found"

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest ::
  Request
  -> IO ( Either Error RqType )
mkRequest req =
  case pathInfo req of
    [topic, "add"]  -> mkAddRequest topic <$> strictRequestBody req
    [topic, "view"] -> pure (mkViewRequest topic)
    ["list"]        -> pure mkListRequest
    _               -> pure (Left InvalidPath)
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest ::
  RqType
  -> Either Error Response
handleRequest = pure . \case
  AddRq _ _ -> resp200 PlainText "Adding comments is not implemented yet"
  ViewRq _  -> resp200 PlainText "Viewing comments is not implemented yet"
  ListRq    -> resp200 PlainText "Listing topics is not implemented yet"

-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app :: Application
app clientReq respond = do
  domainRequest <- mkRequest clientReq
  respond (processDomainRequest domainRequest)

  where
    processDomainRequest ::
      Either Error RqType
      -> Response
    processDomainRequest req =
      let domainResponse = req >>= handleRequest
      in either mkErrorResponse id domainResponse

runApp :: IO ()
runApp = run 4242 app
