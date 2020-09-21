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
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as B

import           Network.Wreq                   ( basicAuth
                                                , getWith
                                                , postWith
                                                , defaults
                                                , auth
                                                , header
                                                , manager
                                                , responseBody
                                                , FormParam((:=))
                                                )
import           Control.Lens                   ( (&)
                                                , (^.)
                                                , (.~)
                                                , (?~)
                                                )
import           Data.Aeson.Lens                ( key
                                                , AsPrimitive(_String)
                                                )
import           Network.HTTP.Client.TLS        ( mkManagerSettings )
import           Network.Connection             ( TLSSettings(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

data Jenkins = Jenkins { username :: String, password :: String , sidbox :: String } deriving (Data,Typeable,Show,Eq)

jenkins :: Jenkins
jenkins =
  Jenkins
      { username =
        def &= help "The username to use to connect to Jenkins" &= typ
          "USERNAME"
      , password =
        def &= help "The password to use to connect to Jenkins" &= typ
          "PASSWORD"
      , sidbox   = def &= help "The name of the sidbox to destroy" &= typ
                     "CORE-1234"
      }
    &= verbosity
    &= help "Destroy a sidbox using Jenkins"
    &= summary "Sniffles-Destroy v0.0.1, (C) Vincibean"

main :: IO ()
main = do
  params <- cmdArgs jenkins
  let usr = B.pack $ username params
  let psw = B.pack $ password params
  let box = sidbox params
  let
    opts =
      defaults
        &  manager
        .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
        &  auth
        ?~ basicAuth usr psw
        &  header "Cookie"
        .~ [ "datadome=KuVut3_YUoTXhjwlLAdY-qzbJiCVSAYU_8WFZvh.-GqirjO1j9JfyYbeQyJNHI67khneJA.SFNWSjGo-CrPfcFd_pYsDYAABEs_Z.Q6D1~; JSESSIONID.3e635b8a=node012lyzma8q05571j9k2pv48ri9113977159.node0"
           ]
  r <- getWith opts "https://jenkins.otrl.io/crumbIssuer/api/json"
  let crumb = r ^. responseBody . key "crumb" . _String
  r' <- postWith
    opts
    "https://jenkins.otrl.io/job/sniffles-destroy/build"
    [ "name" := T.pack "BUILD_NAME"
    , "value" := T.pack box
    , "Jenkins-Crumb" := crumb
    , "json" := T.pack ( "{'parameter':{'name':'BUILD_NAME','value':'" <> box <> "'},'statusCode':'303','redirectTo':'.','Jenkins-Crumb':'" <> T.unpack crumb <> "'}" )
    ]
  print r'
