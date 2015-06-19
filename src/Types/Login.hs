module Types.Login where

import qualified Data.Text as T

data Login =
  Login {
    loginUsername :: T.Text
  , loginPassword :: T.Text
  , loginRemember :: Bool
  } deriving (Eq, Show)
