{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Data.Maybe
import Data.Text
import Yesod

import Murrentaja

data Input = Input {
  text :: Text
  -- flags here
}

data App = App

mkYesod "App" [parseRoutes|
  / HomeR GET POST
|]

instance Yesod App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

murrentajaForm = renderDivs form where
  form :: AForm Handler Textarea
  form = areq textareaField "Input" Nothing

getHomeR :: Handler Html
getHomeR = do
  ((res, widget), enctype) <- runFormPost murrentajaForm
  defaultLayout $ do
    [whamlet|
      <p>
      <form method=post action=@{HomeR} enctype=#{enctype}>
        ^{widget}
        <button> Submit
    |]
    [whamlet|
      <p>#{showResult res}
    |]
    where
    showResult (FormSuccess textArea) =
      transformText (fromFeatures [SpecialGemination]) $ unTextarea textArea
    showResult x = pack $ show x

postHomeR = getHomeR

main = warp 3001 App
  
