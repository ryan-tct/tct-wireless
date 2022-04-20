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
module Handler.Towers.Comments where

import Import hiding (Value)
import Yesod.Form.Bootstrap5 (BootstrapFormLayout (..)
                             , renderBootstrap5
                             , BootstrapGridOptions(..)
                             )
import Helper.Model hiding ((==.))
import Helper.Html
import DoubleLayout
import Text.Read (read)
import Handler.Comment (commentsListWidget, commentModalWidget, commentForm)

getAllTowerComments :: TowerId -> DB [Entity TowerComment]
getAllTowerComments tId = selectList [TowerCommentTowerId ==. tId] [Desc TowerCommentUpdatedAt]

commentsList :: TowerId -> Widget
commentsList tId = do
  allTowerComments <- handlerToWidget $ runDB $ getAllTowerComments tId
  let allComments = map fromTowerComment allTowerComments
  commentModalWidget Nothing (TowerCommentsR tId) (TowerR tId)
  commentsListWidget allComments
  where
    fromTowerComment :: Entity TowerComment -> Entity Comment
    fromTowerComment (Entity _ tc) = (Entity cKey c)
      where
        cKey = towerCommentCommentId tc
        c = Comment (towerCommentMessage tc) (towerCommentUpdatedAt tc)

getTowerCommentsR :: TowerId -> Handler Html
getTowerCommentsR tId = do
  doubleLayout $ do
    setTitle "Tower Comments"
    [whamlet|^{commentsList tId}|]
      
postTowerCommentsR :: TowerId -> Handler Html
postTowerCommentsR tId = do
  ((result, _), _) <- runFormPost $ commentForm Nothing
  case result of
    (FormSuccess tc) -> do
      cId <- runDB $ insert $ tc
      _ <- runDB $ rawExecute "INSERT INTO tower_comment (towerid, commentid) values (?,?)"
             [PersistText (keyToText tId), PersistText (keyToText cId)]
      setMessage "Comment added."
      redirect $ TowerR tId
    _ -> doubleLayout [whamlet|Something went wrong!|]

