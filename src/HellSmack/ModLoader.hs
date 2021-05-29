module HellSmack.ModLoader
  ( VersionQuery (..),
  )
where

data VersionQuery = LatestVersion | RecommendedVersion | ConcreteVersion Text
  deriving stock (Show, Generic)
