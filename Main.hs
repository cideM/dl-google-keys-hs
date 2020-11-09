{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception.Safe
import Control.Monad (guard)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import Data.Void (Void)
import qualified GHC.Generics as G
import qualified Network.HTTP.Req as Req
import qualified Replace.Megaparsec as M
import qualified System.Environment
import System.Exit (ExitCode (..), exitWith)
import System.IO.Error (isDoesNotExistError)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char.Lexer as L
import Text.URI (ParseException, URI, mkURI)

-- | Turn the exception style function from Req into an Either
parseURL :: (Monad m, MonadCatch m) => Text -> m (Either Text URI)
parseURL s = do
  tryURI <- try (mkURI s)
  return $ case (tryURI :: Either ParseException URI) of
    Left e -> Left (pack (show e))
    Right v -> Right v

data Config = Config URI Text deriving (Show)

-- | This gets stored on disk so we can skip network requests if the cache is
-- still valid
data KeysCache = KeysCache
  { expires :: Time.UTCTime,
    keys :: Map Text Text
  }
  deriving (G.Generic, Show)

instance Aeson.FromJSON KeysCache

instance Aeson.ToJSON KeysCache

data CacheState
  = ValidUntil Time.UTCTime
  | NoCacheOnDisk
  | Expired Time.UTCTime
  deriving (Show)

type Parser = M.Parsec Void Text

maxAgeParser :: Parser Integer
maxAgeParser = do
  _ <- "max-age="
  L.decimal

-- | Try to find and parse the max-age= part of a cache-control header and
-- return an actual time value
getMaxAge :: Text -> Either Text Time.NominalDiffTime
getMaxAge input = do
  (_, found, _) <- case M.breakCap maxAgeParser input of
    Nothing -> Left "Couldn't find \"max-age=\" in given text"
    Just success -> Right success
  return (fromInteger found :: Time.NominalDiffTime)

main :: IO ()
main = do
  Config url cache <- getOptions

  -- Catch file does not exist exceptions, let the rest bubble up and crash the
  -- program
  tryCache <-
    tryJust
      (guard . isDoesNotExistError)
      (LBS.readFile (unpack cache))

  now <- Time.getCurrentTime

  (tryGetCacheState :: Either Text CacheState) <- return $ case tryCache of
    Left _ -> Right NoCacheOnDisk
    Right cached -> case Aeson.eitherDecode cached of
      Left err -> Left (pack err)
      Right decodedCache ->
        let expires' = expires (decodedCache :: KeysCache)
            state =
              if now >= expires'
                then Expired expires'
                else ValidUntil expires'
         in Right state

  case tryGetCacheState of
    Left err -> do print err; exitWith (ExitFailure 1)
    Right (ValidUntil time) -> print $ "Cached keys are valid until: " <> show time
    Right NoCacheOnDisk -> do
      print "No cached keys found, downloading new keys..."
      updateCache url cache
    Right (Expired time) -> do
      print "Cache has expired, downloading new keys..."
      print $ "Cache was valid until: " <> show time
      updateCache url cache
  where
    updateCache url cachePath = do
      now <- Time.getCurrentTime

      (uri, _) <- case Req.useHttpsURI url of
        Nothing -> do print "Couldn't turn URL into URI"; exitWith (ExitFailure 1)
        Just success -> return success

      (tryResponse :: Either Req.HttpException (Req.JsonResponse (Map Text Text))) <-
        try $
          Req.runReq Req.defaultHttpConfig $
            Req.req Req.GET uri Req.NoReqBody Req.jsonResponse mempty

      response <- case tryResponse of
        Left err -> do
          print $ "Error during request: " <> show err
          exitWith (ExitFailure 1)
        Right success -> return success

      -- What if it can't be decoded? Does it throw ParseException?
      let body = (Req.responseBody response :: Map Text Text)

      case Req.responseHeader response "Cache-Control" of
        Nothing -> do
          print "No 'Cache-Control' header in response"
          exitWith (ExitFailure 1)
        Just cc -> case getMaxAge (decodeUtf8 cc) of
          Left err -> do print err; exitWith (ExitFailure 1)
          Right maxAgeParsed -> do
            let validUntil = Time.addUTCTime maxAgeParsed now
                encoded = Aeson.encode (KeysCache validUntil body)
            LBS.writeFile (unpack cachePath) encoded
            print $ "Stored new keys at: " <> cachePath

    getOptions =
      let onFail err = do print err; exitWith (ExitFailure 1)
       in do
            urlUnparsed <- pack <$> System.Environment.getEnv "PUBLIC_KEYS_URL"

            cache <- pack <$> System.Environment.getEnv "KEYS_CACHE_PATH"

            tryParseUrl <- parseURL urlUnparsed

            url <- either onFail return tryParseUrl

            return (Config url cache)
