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
import Data.Conduit.Binary
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

getAllAPBackups :: DB [(Single FileStoreId,Single UTCTime,Single Text)]
getAllAPBackups = rawSql "SELECT backupid,updated_at,filename FROM aps_backups ORDER BY updated_at DESC" []

getBackupR :: FileStoreId -> Handler TypedContent
getBackupR bId = do
 b <- runDB $ get404 bId
 addHeader "Content-Disposition" $ T.concat
   [ "attachment; filename=\"", (fileStoreFilename b), "\""]
 sendResponse (T.encodeUtf8 (fileStoreContentType b), toContent (fileStorePayload b))

getFileStoreR :: FileStoreId -> Handler TypedContent
getFileStoreR fsId = do
 fs <- runDB $ get404 fsId
 addHeader "Content-Disposition" $ T.concat
   [ "attachment; filename=\"", (fileStoreFilename fs), "\""]
 sendResponse (T.encodeUtf8 (fileStoreContentType fs), toContent (fileStorePayload fs))

getBackupsR :: Handler Html
getBackupsR = do
  allBackups <- runDB getAllAPBackups
  doubleLayout $ do
    setTitle "Backups"
    $(widgetFile "backups/backupsAPTable")

postBackupsR :: Handler Html
postBackupsR = undefined

data BackupForm = BackupForm
  { fileInfo :: FileInfo
  , createdAt :: UTCTime
  }
  
backupForm :: Form BackupForm
backupForm = renderBootstrap5 bootstrapH $ BackupForm
  <$> fileAFormReq "Choose a file"
  <*> (getCurrentTime |> liftIO |> lift)

backupFormWidget :: Widget
backupFormWidget = $(widgetFile "backups/backupForm")

backupsTableWidget allBackups = $(widgetFile "backups/backupsTable")
