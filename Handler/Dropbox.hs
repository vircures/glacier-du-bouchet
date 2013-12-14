module Handler.Dropbox where

import Import
import Web.Authenticate.OAuth
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.HTTP.Conduit
import Data.Typeable
import Text.Printf (printf)

getDropboxR :: Handler Html
getDropboxR = 
        defaultLayout $ do
            let oauth = mkConsumer (C.pack "5aavmwd791ml126") (C.pack "bjlks0ypu5y4f0y")
            cred <- lift $ authorize oauth
            [whamlet|<p>Temporary credential for #{show oauth} with type #{show cred}...!|]

postDropboxR :: Handler Html
postDropboxR = error "Not yet implemented: postDropboxR"


server = "dropbox"
apiURI = "api.dropbox.com/1/oauth"

{-
Based on sample from
https://github.com/pasberth/ftap/blob/0d39e09194bdb8b32015250c54fd04b3741ed1e2/Ftap/Transfer.hs
 -}
mkConsumer :: B.ByteString -> B.ByteString -> OAuth
mkConsumer consumerKey consumerSecret = newOAuth
    { oauthServerName = server
    , oauthRequestUri = "https://" ++ apiURI ++ "/request_token"
    , oauthAccessTokenUri = "https://" ++ apiURI ++ "/access_token"
    , oauthAuthorizeUri = "https://" ++ apiURI ++ "/authorize"
    , oauthSignatureMethod = HMACSHA1
    , oauthConsumerKey = consumerKey
    , oauthConsumerSecret = consumerSecret
    , oauthVersion         = OAuth10a
    }

--authorize :: OAuth -> (Credential -> IO BS.ByteString) -> IO Credential
--authorize oauth getVerifier = do
authorize :: OAuth -> IO Credential
authorize oauth = do
  tmp <- withManager $ getTemporaryCredential oauth
  return tmp
--  verifier <- getVerifier tmp
--  let tmp' = injectVerifier verifier tmp
--  cred <- withManager $ getTokenCredential oauth tmp'
--  return cred

main :: IO ()
main = do
    print "Start ... "
    let oauth = mkConsumer (C.pack "5aavmwd791ml126") (C.pack "bjlks0ypu5y4f0y")
    cred <- authorize oauth
--    print cred
    putStrLn $ show cred