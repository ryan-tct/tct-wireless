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
module Handler.NavigationTree where

import Import hiding (Value)
import Database.Esqueleto.Experimental as E
import Handler.Towers as T
import Handler.AccessPoints as AP

getNavigationTreeR :: Handler Html
getNavigationTreeR = defaultLayout $ do
  setTitle "NavigationTree"
  [whamlet|^{showTowerLinks}|]

showTowerLinks :: Widget
showTowerLinks = do
  allTowers <- handlerToWidget $ runDB T.getTowerNames
  [whamlet|
<ul>
  $forall (Value tId, Value name) <- allTowers
    <li><a href=@{TowerR tId}>#{name}
      ^{showAPLinks tId}
          |]

showAPLinks :: TowerId -> Widget
showAPLinks towerId = do
  allAPs <- handlerToWidget $ runDB $ AP.getAPNamesFor towerId
  [whamlet|
<ul>
  $forall (Value apId, Value name) <- allAPs
    <li><a href="">#{name}
          |]

-- showAPLinks :: Value TowerId -> Widget
-- showAPLinks evalTK = do
--   allAPs <- handlerToWidget $ runDB $ AP.getAPNamesFor evalTK
--   [whamlet|
-- <ul>
--   $forall (Value apId, Value name) <- allAPs
--     <li><a href="">#{name}
--           |]
