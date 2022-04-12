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
import Data.Text as T

getAllAPBackups :: AccessPointId -> DB [(Single BackupId,Single UTCTime,Single ByteString,Single Text)]
getAllAPBackups apId = rawSql "SELECT backupid,updated_at,payload,filename FROM aps_backups WHERE apid=?" [PersistText (keyToText apId)]

--TODO: Fix this. We need to store the content type I think.
--See:
--https://www.schoolofhaskell.com/school/advanced-haskell/building-a-file-hosting-service-in-yesod/part%204#file-download-links
--https://www.schoolofhaskell.com/school/advanced-haskell/building-a-file-hosting-service-in-yesod/part%204#file-download-links
getBackupR :: AccessPointId -> BackupId -> Handler TypedContent
getBackupR apId bId = undefined
--  do
--  b <- runDB $ get404 bId
--  addHeader "Content-Disposition" $ T.concat
--    [ "attachment; filename=\"", (backupFilename b), "\""]
--  sendResponse
  
--  doubleLayout $ do
--    setTitle "Backup"
 -- StoredFile filename contentType bytes <- getById ident
 --    addHeader "Content-Disposition" $ Text.concat
 --        [ "attachment; filename=\"", filename, "\""]
 --    sendResponse (Text.encodeUtf8 contentType, toContent bytes)

getAPBackupsR :: AccessPointId -> Handler Html
getAPBackupsR apId = do
  (widget, enctype) <- generateFormPost apBackupForm
  allBackups <- runDB $ getAllAPBackups apId
  doubleLayout $ do
    setTitle "Backups"
    [whamlet|
<h2 class="text-center">Backups
<div class="mt-3 mb-3">
  <button type="button" class="btn btn-primary" data-bs-toggle="modal" data-bs-target="#newModal">New Backup
<table class="table table-striped table-responsive mb-3 mt-3">
  <thead>
    <tr>
      <th>Name
      <th>Created At
  <tbody>
    $forall (Single bId, Single createdAt, _, Single filename) <- allBackups
      <tr>
        <td><a href=@{BackupR apId bId}>#{filename}
        <td>#{show $ createdAt}
<div class="modal fade" id="newModal" tabindex="-1" aria-labelledby="nameModalLabel" aria-hidden="true">
  <div class="modal-dialog">
    <div class="modal-content">
      <div class="modal-header">
        <h5 class="modal-title" id="nameModalLabel">New Backup</h5>
        <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
      <div class="modal-body">
        <form class="form mt-3 mb-3" method=POST action=@{APBackupsR apId} enctype=#{enctype}>
          ^{widget}
          <div class="form mt-3 mb-3">
            <button class="btn btn-warning" type="button" data-bs-dismiss="modal"">Close
            <button class="btn btn-primary" type="submit" name="action" value="save">Save Backup
    |]

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
      bId <- runDB $ insert $ Backup myFN (bytes |> toStrict) myUpdatedAt
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
