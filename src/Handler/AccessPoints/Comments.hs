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
module Handler.AccessPoints.Comments where

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

getAllAPComments :: AccessPointId -> DB [Entity AccessPointComment]
getAllAPComments apId = selectList [AccessPointCommentAccessPointId ==. apId]
                                   [Desc AccessPointCommentUpdatedAt]

commentsList :: AccessPointId -> Widget
commentsList apId = do
  allAPComments <- handlerToWidget $ runDB $ getAllAPComments apId
  let allComments = map fromAPComment allAPComments
  commentModalWidget Nothing (APCommentsR apId) (AccessPointR apId)
  commentsListWidget allComments
  where
    fromAPComment :: Entity AccessPointComment -> Entity Comment
    fromAPComment (Entity _ apc) = (Entity cId c)
      where
        cId = accessPointCommentCommentId apc
        c = Comment (accessPointCommentMessage apc) (accessPointCommentUpdatedAt apc)

getAPCommentsR :: AccessPointId -> Handler Html
getAPCommentsR apId = do
  doubleLayout $ do
    setTitle "Comments"
    [whamlet|^{commentsList apId}|]

postAPCommentsR :: AccessPointId -> Handler Html
postAPCommentsR apId = do
  ((result, _), _) <- runFormPost $ commentForm Nothing
  case result of
    FormSuccess apc -> do
      cId <- runDB $ insert $ apc
      _ <- runDB $ rawExecute "INSERT INTO ap_comment (apid, commentid) values (?,?)"
                              [PersistText (keyToText apId), PersistText (keyToText cId)]
      setMessage "Comment added."
      redirect $ AccessPointR apId
    _ -> doubleLayout [whamlet|Something went wrong!|]
