{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.AccessPointTypes where

import Import
import Yesod.Form.Bootstrap4 --(BootstrapFormLayout (..), renderBootstrap4)

getAccessPointTypesR :: Handler Html
getAccessPointTypesR = do
  allAPTypes <- runDB $ getAllAPTypes
  defaultLayout $ do
    setTitle "AP Types"
    [whamlet|
<h1 class="display-1 text-center">Access Point Types
<a href=@{NewAccessPointTypeR} class="btn btn-outline-primary">New AP Type
<table class="table table-striped table-responsive mb-3 mt-3">
  <thead>
    <tr>
      <th>Name
  <tbody>
    $forall Entity aptId apt <- allAPTypes
      <tr>
        <td><a href=@{AccessPointTypeR aptId}>#{accessPointTypeName apt}
    |]
--    $(widgetFile "ap_types/ap_types")

getAccessPointTypeR :: AccessPointTypeId -> Handler Html
getAccessPointTypeR aptId = do
  apt <- runDB $ get404 aptId
  defaultLayout $ do
    setTitle "Acess Point Type"
    [whamlet|
<h1 class="display-1 text-center">#{accessPointTypeName apt}
<div class="btn-group mt-3 mb-3" role="group">
  <a href=@{EditAccessPointTypeR aptId} class="btn btn-outline-primary" role="button">Edit #{accessPointTypeName apt}
<table class="table table-responsive table-bordered table-striped mt-3 mb-3">
  <tbody>
    <tr>
      <th scope="row">Name
      <td>#{accessPointTypeName apt}
    |]

getNewAccessPointTypeR :: Handler Html
getNewAccessPointTypeR = do
  (widget, enctype) <- generateFormPost (accessPointTypeForm Nothing)
  defaultLayout $ do
    setTitle "New Access Point Type"
    [whamlet|
<h1 class="display-1 text-center">New Access Point Type
<form class="form" method=POST action=@{NewAccessPointTypeR} enctype=#{enctype}>
  <div class="form mt-3 mb-3">
    <div class="btn-group" role="group">
      <a href=@{AccessPointTypesR} class="btn btn-outline-warning">Discard Changes
      <button type="submit" class="btn btn-outline-primary">Create AP Type
  ^{widget}
    |]

postNewAccessPointTypeR :: Handler Html
postNewAccessPointTypeR = do
  ((result, _), _) <- runFormPost (accessPointTypeForm Nothing)
  case result of
    FormSuccess apt -> do 
      aptId <- runDB $ insert apt
      -- TODO: Add message that this was created correctly.
      redirect AccessPointTypesR
-- TODO: FIXME
    _ -> defaultLayout [whamlet|Something went wrong!|]

getEditAccessPointTypeR :: AccessPointTypeId -> Handler Html
getEditAccessPointTypeR aptId = do
  apt <- runDB $ get404 aptId
  (widget, enctype) <- generateFormPost (accessPointTypeForm (Just apt))
  defaultLayout $ do
    setTitle $ "Edit "  <> toHtml (accessPointTypeName apt)
    [whamlet|
<h1 class="display-1 text-center">Edit #{accessPointTypeName apt}
<form class="form mt-3 mb-3" method=POST action=@{EditAccessPointTypeR aptId} enctype=#{enctype}>
  ^{widget}
  <div class="form mt-3 mb-3">
    <div class="btn-group" role="group">
      <button class="btn btn-danger" type="submit" name="action" value="delete">Delete #{accessPointTypeName apt}
      <button class="btn btn-warning" type="submit" name="action" value="discard-changes">Discard Changes
      <button class="btn btn-primary" type="submit" name="action" value="save">Save Changes
    |]

postEditAccessPointTypeR :: AccessPointTypeId -> Handler Html
postEditAccessPointTypeR aptId = do
  ((result, _), _) <- runFormPost (accessPointTypeForm Nothing)
  action <- lookupPostParam "action"
  case (result, action) of
    (FormSuccess apt, Just "save") -> do
      runDB $ replace aptId apt
      setMessage $ "Updated access point type"
      redirect AccessPointTypesR
    (FormSuccess _, Just "discard-changes") -> do
      setMessage $ "No changes made."
      redirect AccessPointTypesR
    (FormSuccess _, Just "delete") -> do
      runDB $ delete aptId
      setMessage $ "Deleted access point type"
      redirect AccessPointTypesR
    _ -> defaultLayout $ do
      setMessage "Error editing access point type."
      redirect AccessPointTypesR
  -- case result of
  --   FormSuccess apt -> do
  --     _ <- runDB $ replace aptId apt
  --     redirect AccessPointTypesR
  --   _ -> defaultLayout [whamlet|Something went wrong!|]

getDeleteAccessPointTypeR :: AccessPointTypeId -> Handler Html
getDeleteAccessPointTypeR aptId = do
  apt <- runDB $ get404 aptId
  defaultLayout $ do
    setTitle "Acess Point Type"
    [whamlet|
<h1 class="display-1 text-center">Really Delete #{accessPointTypeName apt}?
<form class="form" method=POST action=@{DeleteAccessPointTypeR aptId}>
<div class="btn-group mt-3 mb-3" role="group">
  <a href=@{AccessPointTypesR} class="btn btn-outline-warning" role="button">Don't Delete
  <button type="submit" class="btn btn-outline-danger">Delete
<table class="table table-responsive table-bordered table-striped mt-3 mb-3">
  <tbody>
    <tr>
      <th scope="row">Name
      <td>#{accessPointTypeName apt}
    |]

postDeleteAccessPointTypeR :: AccessPointTypeId -> Handler Html
postDeleteAccessPointTypeR aptId = undefined

accessPointTypeForm :: Maybe AccessPointType -> Form AccessPointType
accessPointTypeForm mapt = renderBootstrap4 BootstrapBasicForm $ AccessPointType
  <$> areq textField nameSettings (accessPointTypeName <$> mapt)
--  <* bootstrapSubmit (BootstrapSubmit ("Delete" :: Text) "btn btn-danger" [("name", "action"), ("value", "delete")])
--  <* bootstrapSubmit (BootstrapSubmit ("Discard Changes" :: Text) "btn btn-warning" [("name", "action"), ("value", "discard-changes")])
--  <* bootstrapSubmit (BootstrapSubmit ("Save" :: Text) "btn btn-primary" [("name", "action"), ("value", "save")])
  where nameSettings = FieldSettings
          { fsLabel = "Access Point Type"
          , fsTooltip = Nothing
          , fsId = Just "name"
          , fsName = Just "name"
          , fsAttrs =
            [ ("class", "form-control")
            , ("placeholder", "eg: PMP450m")
            ]
          }

getAllAPTypes :: DB [Entity AccessPointType]
getAllAPTypes = selectList [] [Asc AccessPointTypeName]

