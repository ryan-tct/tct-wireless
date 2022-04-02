{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.AccessPoints where

import Import
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

--getAllAccessPoints :: DB [Entity AccessPoint]
--getAllAccessPoints = undefined
