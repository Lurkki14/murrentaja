{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Data.Text
import Yesod

data Input = Input {
  text :: Text
  -- flags here
}

data App = App

mkYesod "App" [parseRoutes|
  / HomeR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

murrentajaForm = renderDivs form where
  form :: AForm Handler Text
  form = areq textField "Input" Nothing

getHomeR :: Handler Html
getHomeR = do
  (widget, enctype) <- generateFormPost murrentajaForm
  defaultLayout $ do
    widget
    [whamlet| <button> Submit |]

main = warp 3001 App
  
