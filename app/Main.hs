{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           System.Console.CmdArgs         ( Data
                                                , Typeable
                                                , (&=)
                                                , cmdArgs
                                                , help
                                                , summary
                                                , typ
                                                , verbosity
                                                , Default(def)
                                                )
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString.Char8         as B

import           Network.Wreq                   ( basicAuth
                                                , defaults
                                                , auth
                                                , responseBody
                                                , Response
                                                , FormParam((:=))
                                                , Options
                                                )
import qualified Network.Wreq.Session          as S
import           Control.Lens                   ( (&)
                                                , (^.)
                                                , (?~)
                                                )
import           Data.Aeson.Lens                ( key
                                                , AsPrimitive(_String)
                                                )
import           Network.HTTP.Client.TLS        ( mkManagerSettings )
import           Network.Connection             ( TLSSettings(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Network.HTTP.Client           as HTTP

data Jenkins = Jenkins { username :: String, password :: String , sidboxes :: [String] } deriving (Data,Typeable,Show,Eq)

jenkins :: Jenkins
jenkins =
  Jenkins
      { username =
        def &= help "The username to use to connect to Jenkins" &= typ
          "USERNAME"
      , password =
        def &= help "The password to use to connect to Jenkins" &= typ
          "PASSWORD"
      , sidboxes = def &= help "The name of the sidbox to destroy" &= typ
                     "CORE-1234"
      }
    &= verbosity
    &= help "Destroy a sidbox using Jenkins"
    &= summary "Sniffles-Destroy v0.0.1, (C) Vincibean"

main :: IO ()
main = do
  params <- cmdArgs jenkins
  sess   <- S.newSessionControl
    (Just (HTTP.createCookieJar []))
    (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  let usr   = B.pack $ username params
  let psw   = B.pack $ password params
  let boxes = sidboxes params
  let opts  = defaults & auth ?~ basicAuth usr psw
  r <- S.getWith opts sess "https://jenkins.otrl.io/crumbIssuer/api/json"
  let crumb = r ^. responseBody . key "crumb" . _String
  r' <- destroy opts sess crumb `traverse` boxes
  print r'

destroy :: Options -> S.Session -> Text -> String -> IO (Response L.ByteString)
destroy opts sess crumb box = S.postWith
  opts
  sess
  "https://jenkins.otrl.io/job/sniffles-destroy/build"
  [ "name" := T.pack "BUILD_NAME"
  , "value" := T.pack box
  , "Jenkins-Crumb" := crumb
  , "json" := T.pack
    (  "{'parameter':{'name':'BUILD_NAME','value':'"
    <> box
    <> "'},'statusCode':'303','redirectTo':'.','Jenkins-Crumb':'"
    <> T.unpack crumb
    <> "'}"
    )
  ]
