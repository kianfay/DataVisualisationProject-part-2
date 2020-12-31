{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Home where

import Import hiding (newManager)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

-- imports separate from template imports
import Data.Aeson
import Handler.GithubRepresent
import Servant.Client
import Network.HTTP.Client          (newManager)
import Network.HTTP.Client.TLS      (tlsManagerSettings)

---
-- Definition for our LangR handler
---

getLangR :: Handler Value
getLangR = do
    x <- liftIO doGithubReq
    case x of
        Left _ -> error "parse error"
        Right lang -> do 
            print $ encode lang 
            return $ toJSON lang  

        ---- NOW AS WE CANT GET FURTHER, TRY CODE MENTORS


-- A separate function which gets the data from the API endpoints
queries :: ClientM Language
queries = do getLangs (Just "Visualisation-App") "microsoft" "VSCode"
    

-- This function now essentially runs the Queries function and prints the output 
doGithubReq ::  IO (Either ClientError Language)
doGithubReq =   let env :: IO Servant.Client.ClientEnv
                    env = do
                                manager <- newManager tlsManagerSettings
                                -- Here we define a ClientEnv smart using the smart constructor, mkClientEnv
                                return $ Servant.Client.mkClientEnv manager (Servant.Client.BaseUrl {baseUrlScheme = Servant.Client.Http, baseUrlHost = "api.github.com", baseUrlPort = 80, baseUrlPath = ""})
                        -- m >>= k suggests "feed the result of computation m to the function k" - from StackOverflow
                        -- We could also put case after in, and we would not write case with a \.
                in (Servant.Client.runClientM (queries) =<< env)

-- TEMPLATE HANDLERS
-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
