module Data.Aeson.Newtypes
  ( Quoted(..)
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Aeson.Parser (value)
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Encoding (encodeUtf8)
import Data.Attoparsec.ByteString (parseOnly)

import qualified Data.Text as T

-- | A newtype wrapper for quoted values.
-- For example, numbers stored as strings in financial applications.
newtype Quoted a = Quoted { unQuoted :: a }
  deriving(Eq, Read, Show)

-- Parse UTF-8 Text to a JSON Value
parseUtf8Text :: FromJSON a => T.Text -> Parser a
parseUtf8Text = (>>= parseJSON)    -- Parser Value -> Parser a
              . either fail return -- Either Error Value -> Parser Value
              . parseOnly value    -- ByteString -> Either Error Value
              . encodeUtf8         -- Text to ByteString

instance FromJSON a => FromJSON (Quoted a) where
  parseJSON (String t) = fmap Quoted (parseUtf8Text t)
  parseJSON x          = fail ("expected quoted Value, got " ++ show x)

instance ToJSON a => ToJSON (Quoted a) where
  toJSON = String . toStrict . toLazyText
         . encodeToTextBuilder . toJSON . unQuoted
