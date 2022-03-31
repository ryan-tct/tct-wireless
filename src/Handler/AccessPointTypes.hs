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
  (widget, enctype) <- generateFormPost (accessPointTypeForm Nothing)
  defaultLayout $ do
    setTitle "AP Types"
--    $(widgetFile "ap_types/ap_types")
    [whamlet|
<h1 class="display-1 text-center">Access Point Types
<table class="table table-striped table-responsive mb-3 mt-3">
  <thead>
    <tr>
      <th>Name
  <tbody>
    $forall Entity aptId apt <- allAPTypes
      <tr>
        <td><a href=@{AccessPointTypeR aptId}>#{accessPointTypeName apt}
<div class="btn-group mt-3 mb-3" role="group">
  <button type="button" class="btn btn-primary" data-bs-toggle="modal" data-bs-target="#editModal">New AP Type
<div class="modal fade" id="editModal" tabindex="-1" aria-labelledby="exampleModalLabel" aria-hidden="true">
  <div class="modal-dialog">
    <div class="modal-content">
      <div class="modal-header">
        <h5 class="modal-title" id="exampleModalLabel">Modal title</h5>
        <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
      <div class="modal-body">
        <form class="form mt-3 mb-3" method=POST action=@{AccessPointTypesR} enctype=#{enctype}>
          ^{widget}
          <div class="form mt-3 mb-3">
            <div class="btn-group" role="group">
              <a href=@{AccessPointTypesR} class="btn btn-warning">Discard Changes
              <button class="btn btn-primary" type="submit" name="action" value="save">Save AP Type
    |]
    
postAccessPointTypesR :: Handler Html
postAccessPointTypesR = do
  ((result, _), _) <- runFormPost (accessPointTypeForm Nothing)
  case result of
    FormSuccess apt -> do 
      _ <- runDB $ insert apt
      setMessage "Access Point Type created."
      redirect AccessPointTypesR
-- TODO: FIXME
    _ -> defaultLayout [whamlet|Something went wrong!|]

getAccessPointTypeR :: AccessPointTypeId -> Handler Html
getAccessPointTypeR aptId = do
  apt <- runDB $ get404 aptId
  (widget, enctype) <- generateFormPost (accessPointTypeForm (Just apt))
  defaultLayout $ do
    setTitle "Acess Point Type"
    [whamlet|
<h1 class="display-1 text-center">#{accessPointTypeName apt}
<table class="table table-responsive table-bordered table-striped mt-3 mb-3">
  <tbody>
    <tr>
      <th scope="row">Name
      <td>#{accessPointTypeName apt}
<div class="btn-group mt-3 mb-3" role="group">
  <button type="button" class="btn btn-primary" data-bs-toggle="modal" data-bs-target="#editModal">Edit
<div class="modal fade" id="editModal" tabindex="-1" aria-labelledby="exampleModalLabel" aria-hidden="true">
  <div class="modal-dialog">
    <div class="modal-content">
      <div class="modal-header">
        <h5 class="modal-title" id="exampleModalLabel">Edit #{accessPointTypeName apt}</h5>
        <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
      <div class="modal-body">
        <form class="form mt-3 mb-3" method=POST action=@{AccessPointTypeR aptId} enctype=#{enctype}>
          ^{widget}
          <div class="form mt-3 mb-3">
            <div class="btn-group" role="group">
              <button class="btn btn-danger" type="submit" name="action" value="delete">Delete #{accessPointTypeName apt}
              <button class="btn btn-warning" type="submit" name="action" value="discard-changes">Discard Changes
              <button class="btn btn-primary" type="submit" name="action" value="save">Save Changes
    |]

postAccessPointTypeR :: AccessPointTypeId -> Handler Html
postAccessPointTypeR aptId = do
  ((result, _), _) <- runFormPost (accessPointTypeForm Nothing)
  action <- lookupPostParam "action"
  case (result, action) of
    (FormSuccess apt, Just "save") -> do
      runDB $ replace aptId apt
      setMessage "Updated access point type"
      redirect $ AccessPointTypeR aptId
    (FormSuccess _, Just "discard-changes") -> do
      setMessage "No changes made."
      redirect $ AccessPointTypeR aptId
    (FormSuccess _, Just "delete") -> do
      runDB $ delete aptId
      setMessage "Deleted access point type"
      redirect AccessPointTypesR
    _ -> defaultLayout $ do
      setMessage "Error editing access point type."
      redirect $ AccessPointTypeR aptId

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

