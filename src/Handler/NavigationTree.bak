{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Handler.NavigationTree where

import Import hiding (Value)
import Database.Esqueleto.Experimental as E
import qualified Data.Text as Text
import Helper.Model

getNavigationTreeR :: Handler Html
getNavigationTreeR = defaultLayout $ do
  setTitle "NavigationTree"
  [whamlet|^{showTree}|]

-- <button .navbar-toggler type="button" data-bs-toggle="collapse" data-bs-target="#left-navbar" aria-expanded="true" aria-controls="navbar">
---   <i .fa-solid.fa-expand>
showTree :: Widget
showTree = do
  [whamlet|
<div>
  <i class="fa-solid fa-solid fa-globe">
  <a href=@{TowersR}>Towers
  ^{showTowerLinks}
          |]
  toWidget
    [lucius|
.nav-tower-list {
}
.btn-navtree-toggle {
  display: inline-flex;
  align-items: center;
  padding: .25rem .5rem;
  font-weight: 600;
  color: rgba(0, 0, 0, 1.0);
  background-color: transparent;
  border: 0;
}
.btn-navtree-toggle::before {
  width: 1.25em;
  line-height: 0;
  content: url("data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 16 16'%3e%3cpath fill='none' stroke='black' stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='M5 14l6-6-6-6'/%3e%3c/svg%3e");
  transition: transform 0.25s ease;
  transform-origin: .5em 50%;
}
.btn-navtree-toggle[aria-expanded="true"]::before {
  transform: rotate(90deg);
}
.btn-navtree-toggle-sub {
  padding-left: 2.0rem;
}
.left-nav {
  min-width: 100px;
  max-width: 400px;
  margin-right: 1rem;
  outline: 1px solid gray;
}
           |]

      
showTowerLinks :: Widget
showTowerLinks = do
  allTowers <- handlerToWidget $ runDB getTowerNames
  [whamlet|
<ul class="list-unstyled ps-0">
  $forall (Value tId, Value name) <- allTowers
    <li>
      <button class="btn btn-toggle btn-navtree-toggle align-items-center rounded collapsed" data-bs-toggle="collapse" data-bs-target=#{"#" ++ (webFormat name)}-collapse aria-expanded="false">
      <i class="fa-solid fa-tower-cell">
      <a href=@{TowerR tId}>#{name}
      <div id=#{webFormat name}-collapse class="collapse" style="">
        ^{showAPLinks tId}
          |]

showAPLinks :: TowerId -> Widget
showAPLinks towerId = do
  allAPs <- handlerToWidget $ runDB $ getAPNamesFor towerId
  [whamlet|
<ul class="list-unstyled btn-navtree-toggle-sub">
  $forall (Value apId, Value name) <- allAPs
    <li>
      <button class="btn btn-toggle btn-navtree-toggle align-items-center rounded collapsed" data-bs-toggle="collapse" data-bs-target=#{"#" ++ (webFormat name)}-collapse aria-expanded="false">
      <i class="fa-solid fa-wifi">
      <a href=@{AccessPointR apId}>#{name}
      <div id=#{webFormat name}-collapse class="collapse" style="">
          |]

webFormat :: Text -> Text
webFormat t = Text.replace " " "-" t

