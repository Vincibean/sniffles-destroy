{-# LANGUAGE OverloadedStrings #-}

module Main where

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
import           Data.Text                      ( Text
                                                , pack
                                                )

main :: IO ()
main = do
  let
    opts =
      defaults
        &  manager
        .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
        &  auth
        ?~ basicAuth "" ""
        &  header "Cookie"
        .~ [ "datadome=KuVut3_YUoTXhjwlLAdY-qzbJiCVSAYU_8WFZvh.-GqirjO1j9JfyYbeQyJNHI67khneJA.SFNWSjGo-CrPfcFd_pYsDYAABEs_Z.Q6D1~; JSESSIONID.3e635b8a=node01gizd1nish1xn7wv7kw1y6j9a12390192.node0"
           ]
  r <- getWith opts "https://jenkins.otrl.io/crumbIssuer/api/json"
  let crumb :: Text
      crumb = r ^. responseBody . key "crumb" . _String
  r' <- postWith
    opts
    "https://jenkins.otrl.io/job/sniffles-destroy/buildWithParameters"
    [ "name" := pack "BUILD_NAME"
    , "value" := pack "wat"
    , "Jenkins-Crumb" := crumb
    ]
  print r'
