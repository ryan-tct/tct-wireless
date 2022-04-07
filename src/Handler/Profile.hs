{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import
import DoubleLayout

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    doubleLayout $ do
        setTitle . toHtml $ wuserIdent user <> "'s User page"
        $(widgetFile "profile")
