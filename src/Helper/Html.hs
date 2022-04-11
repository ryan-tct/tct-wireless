{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell #-}
module Helper.Html where

import Import
import Yesod.Form.Bootstrap5 (BootstrapFormLayout (..)
                             , renderBootstrap5
                             , BootstrapGridOptions(..)
                             )
import Helper.Model (Value(..))

textAreaField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
textAreaField = Field
  { fieldParse = parseHelper $ Right
  , fieldView = \theId fname attrs val isReq ->
    [whamlet|
$newline never
<textarea id="#{theId}" name="#{fname}" *{attrs} type="text" :isReq:required>#{either id id val}
    |]
  , fieldEnctype = UrlEncoded
  }

bootstrapH = BootstrapHorizontalForm
      { bflLabelOffset = ColSm 0
      , bflLabelSize = ColSm 3
      , bflInputOffset = ColSm 0
      , bflInputSize = ColSm 9
      }
