{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE TemplateHaskell     #-}

module Handler.GithubRepresent where 

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text hiding (drop)
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client
import           Data.Aeson.TH

data Org = Org {
    public_repos :: Int
} deriving (Generic, FromJSON, Show)

data Repo = Repo {
    name :: Text
} deriving (Generic, FromJSON, Show)

data Language = Language {
  _TypeScript :: Maybe Int,
  _JavaScript :: Maybe Int,
  _CSS :: Maybe Int,
  _HTML :: Maybe Int,
  _Python :: Maybe Int,
  _Makefile :: Maybe Int,
  _C :: Maybe Int,
  _Pug :: Maybe Int,
  _Go :: Maybe Int,
  _Java :: Maybe Int,
  _Haskell :: Maybe Int
} deriving (Generic, Show, ToJSON)

-- We import Data.Aeson.TH and add the TemplateHaskell extension so that we
-- can specify how we want our FromJSON instance of Language to operate. As
-- the language endpoint of the GitHub API returns the field names as the legitimate
-- language names, starting with a capital letter, we need to provide a workaround
-- so that the Aeson package can understand what the names actually are, and not interpet
-- them as data constructors (which start with a capital letter)
$(deriveFromJSON defaultOptions {
    fieldLabelModifier = drop 1
} ''Language)


-- Here we define our API, outlining the structure of our HTTP requests in a abstracted way
-- The first one gets the Organization endpoint of a given organization, so we can get the number of public repos.
-- The second one gets the repos of a given organization.
type GitHubAPI =     Header "User-Agent" UserAgentHeader
                    :> "orgs"
                    :> Capture "organization" Organization 
                    :> Get '[JSON] Org
                :<|> Header "User-Agent" UserAgentHeader
                    :> "orgs"
                    :> Capture "organization" Organization 
                    :> "repos"
                    :> QueryParam "per_page" Int
                    :> Get '[JSON] [Repo]
                :<|> Header "User-Agent" UserAgentHeader
                    :> "repos"
                    :> Capture "organization" Organization 
                    :> Capture "repo" RepoName 
                    :> "languages"
                    :> Get '[JSON] Language

-- We associate our type with this proxy type variable so that we can pass the type to Servant
gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

-- Define the types of the different api functions (to translate gitHubAPI to Haskell functions)
type Organization = Text
type UserAgentHeader = Text
type RepoName = Text
type PerPage = Int

getOrg :: Maybe UserAgentHeader -> Organization -> ClientM Org 

getRepos :: Maybe UserAgentHeader -> Organization -> Maybe PerPage -> ClientM [Repo]

getLangs :: Maybe UserAgentHeader -> Organization -> RepoName -> ClientM Language

getOrg :<|> getRepos :<|> getLangs = client gitHubAPI