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
module DoubleLayout where

import Import hiding (Value)
import Text.Hamlet (hamletFile)
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI

import Handler.NavigationTree

doubleLayout :: Widget -> Handler Html
doubleLayout widget = do
  master <- getYesod
  mmsg <- getMessage

  muser <- maybeAuthPair
  mcurrentRoute <- getCurrentRoute

  -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
  (title, parents) <- breadcrumbs

  -- Define the menu items of the header.
  let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Home"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Towers"
                    , menuItemRoute = TowersR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Access Points"
                    , menuItemRoute = AccessPointsR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Equipment"
                    , menuItemRoute = EquipmentsR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Profile"
                    , menuItemRoute = ProfileR
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Login"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                ]
  

  let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
  let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

  let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
  let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

  -- We break up the default layout into two components:
  -- default-layout is the contents of the body tag, and
  -- default-layout-wrapper is the entire page. Since the final
  -- value passed to hamletToRepHtml cannot be a widget, this allows
  -- you to use normal widget features in default-layout.

  pc <- widgetToPageContent $ do
    addStylesheet $ StaticR css_bootstrap_css
    addStylesheet $ StaticR css_bootstrap_table_min_css
    addStylesheet $ StaticR fonts_css_all_css
    addScript $ StaticR js_bootstrap_bundle_js
    addScript $ StaticR js_bootstrap_table_min_js
    $(widgetFile "double-layout")
  withUrlRenderer $(hamletFile "templates/double-layout-wrapper.hamlet")
