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
module Handler.Comment where

import Import
import DoubleLayout
import Helper.Html
import Yesod.Form.Bootstrap5 (BootstrapFormLayout (..)
                             , renderBootstrap5
                             , BootstrapGridOptions(..)
                             )
import CMark
import Text.Blaze.Html
import Text.HTML.SanitizeXSS

htmlEscape t = t |> sanitize |> commonmarkToHtml [] |> preEscapedToHtml

getCommentR :: CommentId -> Handler Html
getCommentR cId = do
  c <- runDB $ get404 cId
  doubleLayout $ do
    setTitle "Comment"
    commentViewWidget (Just c) (CommentR cId) (CommentR cId)

commentViewWidget mc postR discR = do
  $(widgetFile "comments/commentView")

postCommentR :: CommentId -> Handler Html
postCommentR cId = do
  ((result, _), _) <- runFormPost $ commentForm Nothing
  action <- lookupPostParam "action"
  case (result, action) of
    (FormSuccess c, Just "save") -> do
      runDB $ replace cId c
      setMessage "Updated comment"
      redirect HomeR
    (FormSuccess _, Just "delete") -> do
      runDB $ delete cId
      setMessage "Deleted comment."
      redirect HomeR
    _ -> doubleLayout $ do
      setMessage "Error editing comment"
      redirect HomeR

getCommentsR :: Handler Html
getCommentsR = do
  allComments <- runDB $ getAllComments
  doubleLayout $ do
    setTitle "Comments"
--    commentModalWidget Nothing CommentsR CommentsR
    commentsListWidget allComments

postCommentsR :: Handler Html
postCommentsR = do
  ((result, _), _) <- runFormPost $ commentForm Nothing
  case result of
    FormSuccess c -> do
      runDB $ insert c
      setMessage "Inserted comment, but this is probably not what you wanted to do as the comment is not associated with any object."
      redirect HomeR
    _ -> do
      setMessage "Error adding comment."
      redirect HomeR

getAllComments :: DB [Entity Comment]
getAllComments = selectList [] [Desc CommentUpdatedAt]

commentsListWidget :: Foldable t => t (Entity Comment) -> Widget
commentsListWidget allComments = $(widgetFile "comments/commentsList")

commentModalWidget :: Maybe Comment -> Route App -> Route App -> Widget
commentModalWidget mc postR discR = $(widgetFile "comments/commentModal")

commentFormWidget :: Maybe Comment -> Route App -> Route App -> Widget
commentFormWidget mc postR discR = do
  (formWidget, formEncType) <- handlerToWidget $ generateFormPost $ commentForm mc
  $(widgetFile "comments/commentForm")

commentForm :: Maybe Comment -> Form Comment
commentForm mc = renderBootstrap5 bootstrapH $ Comment
  <$> areq textAreaField messageSettings (commentMessage <$> mc)
  <*> (getCurrentTime |> liftIO |> lift)
  where messageSettings = FieldSettings
          { fsLabel = "Message"
          , fsTooltip = Nothing
          , fsId = Just "message"
          , fsName = Just "message"
          , fsAttrs =
            [ ("class", "form-control")
            ]
          }
