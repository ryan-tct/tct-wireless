{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Backups where

import Import hiding (Value)
import Yesod.Form.Bootstrap5 (BootstrapFormLayout (..)
                             , renderBootstrap5
                             , BootstrapGridOptions(..)
                             )
import DoubleLayout
import Helper.Model
import Helper.Html

import Database.Esqueleto.Experimental as E hiding(delete, isNothing)
import Data.Conduit.Binary
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

getAllAPBackups :: AccessPointId -> DB [(Single BackupId,Single UTCTime,Single Text)]
getAllAPBackups apId = rawSql "SELECT backupid,updated_at,filename FROM aps_backups WHERE apid=? ORDER BY updated_at DESC" [PersistText (keyToText apId)]

getAllBackups :: DB [(Single BackupId,Single UTCTime,Single Text)]
getAllBackups = rawSql "SELECT backupid,updated_at,filename FROM aps_backups ORDER BY updated_at DESC" []

getBackupR :: BackupId -> Handler TypedContent
getBackupR bId = do
 b <- runDB $ get404 bId
 addHeader "Content-Disposition" $ T.concat
   [ "attachment; filename=\"", (backupFilename b), "\""]
 sendResponse (T.encodeUtf8 (backupContentType b), toContent (backupPayload b))

getAPBackupsR :: AccessPointId -> Handler Html
getAPBackupsR apId = do
  (widget, enctype) <- generateFormPost apBackupForm
  allBackups <- runDB $ getAllAPBackups apId
  doubleLayout $ do
    setTitle "Backups"
    $(widgetFile "backups/apBackups")

postAPBackupsR :: AccessPointId -> Handler Html
postAPBackupsR apId = do
  ((result, _), _) <- runFormPost apBackupForm
  case result of
    FormSuccess b -> do
      --TODO: Clean this up
      bytes <- connect (b |> fileInfo |> fileSource) sinkLbs
      let
        myFN = fileName $ fileInfo b
        myUpdatedAt = createdAt b
        myContentType = fileContentType $ fileInfo b
      bId <- runDB $ insert $ Backup myFN myContentType (bytes |> toStrict) myUpdatedAt
      _ <- runDB $ rawExecute "insert into accesspoint_backup (accesspointid, backupid) values (?,?)" [PersistText (keyToText apId), PersistText (keyToText bId)]
      setMessage "Backup saved."
      redirect $ APBackupsR apId
    _ -> doubleLayout [whamlet|Someting went wrong!|]

data BackupForm = BackupForm
  { fileInfo :: FileInfo
  , createdAt :: UTCTime
  }
  
apBackupForm :: Form BackupForm
apBackupForm = renderBootstrap5 bootstrapH $ BackupForm
  <$> fileAFormReq "Choose a file"
  <*> (getCurrentTime |> liftIO |> lift)

getBackupsR :: Handler Html
getBackupsR = do
  allBackups <- runDB getAllBackups
  doubleLayout $ do
    setTitle "Backups"
    $(widgetFile "backups/backupsTable")

postBackupsR :: Handler Html
postBackupsR = undefined

