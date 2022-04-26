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
module Handler.Equipment.Comments where

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

getAllEquipmentComments :: EquipmentId -> DB [Entity EquipmentComment]
getAllEquipmentComments eId = selectList [EquipmentCommentEquipmentId ==. eId]
                                         [Desc EquipmentCommentUpdatedAt]

commentsList :: EquipmentId -> Widget
commentsList eId = do
  allEquipmentComments <- handlerToWidget $ runDB $ getAllEquipmentComments eId
  let allComments = map fromEqComment allEquipmentComments
  commentModalWidget Nothing (EqCommentsR eId) (EquipmentR eId)
  commentsListWidget allComments
  where
    fromEqComment :: Entity EquipmentComment -> Entity Comment
    fromEqComment (Entity _ ec) = (Entity cId c)
      where
        cId = equipmentCommentCommentId ec
        c = Comment (equipmentCommentMessage ec) (equipmentCommentUpdatedAt ec)

getEqCommentsR :: EquipmentId -> Handler Html
getEqCommentsR eId = do
  doubleLayout $ do
    setTitle "Comments"
    [whamlet|^{commentsList eId}|]

postEqCommentsR ::EquipmentId -> Handler Html
postEqCommentsR eId = do
  ((result, _), _) <- runFormPost $ commentForm Nothing
  case result of
    FormSuccess ec -> do
      cId <- runDB $ insert $ ec
      _ <- runDB $ rawExecute "INSERT INTO equipment_comment (equipmentid, commentid) VALUES (?,?)"
                              [PersistText (keyToText eId), PersistText (keyToText cId)]
      setMessage "Comment added."
      redirect $ EquipmentR eId
    _ -> doubleLayout [whamlet|Something went wrong!|]
