module Main where

import Data.Text (Text)
import Database.PostgreSQL.Simple
    (connectPostgreSQL)
import Servant
    ( Proxy(..),
      serve,
      type (:<|>)(..),
      Capture,
      JSON,
      QueryParam,
      type (:>),
      Get, ReqBody, Post)
import Prelude
import Network.Wai.Handler.Warp ( run )
import Types
import Network
import ProgramOptions
import Options.Applicative (execParser, info)

type KSVAPI = "images" :> Get '[JSON] [Image]
          :<|> "images" :> Capture "imageId" Text :> Get '[JSON] (Maybe Image)
          :<|> "images_detect" :> QueryParam "objects" String :> Get '[JSON] [Image]
          :<|> "upload" :> ReqBody '[JSON] RequestBody :> Post '[JSON] Image

main :: IO ()
main =
    do
    ProgramOptions dbname user password authCode <- execParser (info cmdArgsParser mempty)
    psqlConn <- connectPostgreSQL ("dbname='" <> dbname <> "' user='" <> user <> "' password='" <> password <> "'")
    let server3 = getImages psqlConn
               :<|> getImage psqlConn
               :<|> getImagesofObjectsDetected psqlConn
               :<|> uploadImagefromFile psqlConn authCode
        userAPI = Proxy :: Proxy KSVAPI
    run 8081 (serve userAPI server3)

--TO DO 
-- 1. Proper errorcodes had to be implemented at certain places.
-- 2. COde can be reafctored a bit to look neat.
