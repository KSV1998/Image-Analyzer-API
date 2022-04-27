module Network where

import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple
  ( Response,
    Request,
    getResponseBody,
    httpJSON,
    setRequestHeader,
    setRequestQueryString
  )
import Database.PostgreSQL.Simple
    ( execute,
      query,
      Only(Only),
      Connection )
import Database.PostgreSQL.Simple.Types
    ( PGArray(PGArray, fromPGArray) )
import Data.UUID.V4 ( nextRandom )
import Data.UUID ( UUID, fromText )
import Servant
    ( Handler)
import Prelude
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Concurrent ()
import Safe ( headMay )
import Data.List.Extra ( split )
import qualified Data.Map.Strict as Map
import Data.Maybe ( fromMaybe )
import Network.HTTP.Client.MultipartFormData
import Types

setContentTypeJSON :: Request -> Request
setContentTypeJSON = setRequestHeader "Content-Type" ["application/json"]

setAuthorizationHeader :: AuthCode -> Request -> Request
setAuthorizationHeader authCode =
  setRequestHeader
    "Authorization"
    [authCode]
-- "Basic YWNjXzJjYWEyYTk1MjZkNDZjNTphNGEwYmRlZjJhOGY0MjJjNzlhZDg5NTNiMzc0NGY1MA=="

detectObjectsbyUrl :: AuthCode -> String -> IO IResponse
detectObjectsbyUrl authCode (encodeUtf8 . pack -> uri) = do
  let request =
        setRequestQueryString [("image_url", Just uri)] $
          setContentTypeJSON $
            setAuthorizationHeader
              authCode
              "GET https://api.imagga.com/v2/tags"
  response <- httpJSON request :: IO (Response IResponse)
  return $ getResponseBody response

detectObjectsbyFile :: AuthCode -> String -> IO IResponse
detectObjectsbyFile authCode uri = do
  request <-
        formDataBody [partFileSource "image" uri] $
        setRequestQueryString [] $
          setContentTypeJSON $
            setAuthorizationHeader
              authCode
              "GET https://api.imagga.com/v2/tags"
  response <- httpJSON request :: IO (Response IResponse)
  return $ getResponseBody response

uploadImagefromFile :: Connection -> AuthCode -> RequestBody -> Handler Image 
uploadImagefromFile psqlConn authCode RequestBody{url, label, is_det, source}  =
  liftIO $ do
    uuid <- nextRandom
    objs <- if is_det then fmap pure getObjectNamesFromResponse <$> ((if source == "file" then  detectObjectsbyFile else detectObjectsbyUrl) authCode url) else pure Nothing
    _ <- execute psqlConn "INSERT INTO images_v3 VALUES (?, ?, ?, ?, ?)" (uuid, PGArray <$> objs, url, is_det, (fromMaybe "def-label" label))
    pure $ Image uuid (fromMaybe "def-label" label) is_det objs url

getObjectNamesFromResponse :: IResponse -> [String]
getObjectNamesFromResponse (IResponse (Results res)) = map (\(IData (IName en)) -> en) res

getImages :: Connection -> Handler [Image]
getImages psqlConn =
    do
     x <- liftIO $ query psqlConn "SELECT * FROM images_v3" ()
     pure $ map (\(uid, objs, uri, is_det_enabled, label) -> Image uid label is_det_enabled (Just $ fromPGArray objs) uri) x

getImage :: Connection -> Text -> Handler (Maybe Image)
getImage psqlConn uuid =
    do
     x <- liftIO $ headMay <$> query psqlConn "SELECT * FROM images_v3 where id = ?" (Only (fromText uuid))
     pure $ (\(uid, objs, uri, is_det_enabled, label) -> Image uid label is_det_enabled (Just $ fromPGArray objs) uri) <$> x

getImagesofObjectsDetected :: Connection -> Maybe String -> Handler [Image]
getImagesofObjectsDetected psqlConn (Just k) =
  Map.elems . Map.unions <$> mapM getDetectedImage (split (== ',') k)
  where
    getDetectedImage :: String -> Handler (Map.Map UUID Image)
    getDetectedImage objName =
      do
       x <- liftIO $ query psqlConn "SELECT * FROM images_v3 where ? =ANY(objects)" (Only objName)
       pure $ Map.fromList $ map (\(uid, objs, uri, is_det_enabled, label) -> (uid, Image uid label is_det_enabled (Just $ fromPGArray objs) uri)) x
getImagesofObjectsDetected _ Nothing = pure []
