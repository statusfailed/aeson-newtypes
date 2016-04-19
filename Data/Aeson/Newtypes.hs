module Data.Aeson.Newtypes
  ( Quoted(..)
  , Base64(..)
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Aeson.Parser (value)
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Encoding (encodeUtf8)
import Data.Attoparsec.ByteString (parseOnly)

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64

-- | A newtype wrapper for quoted values.
-- For example, numbers stored as strings in financial applications.
newtype Quoted a = Quoted { unQuoted :: a }
  deriving(Eq, Read, Show)

instance FromJSON a => FromJSON (Quoted a) where
  parseJSON (String t) = fmap Quoted (parseUtf8Text t)
  parseJSON x          = fail ("expected quoted Value, got " ++ show x)

instance ToJSON a => ToJSON (Quoted a) where
  toJSON = String . toStrict . toLazyText
         . encodeToTextBuilder . toJSON . unQuoted

-- | Base64-encoded binary data
--
-- This relies on "UTF-8 encoded base64 text" being the exact same byte sequence
-- as "ASCII-encoded base64 text" because B64 only uses chars in the ascii range.
newtype Base64 = Base64 { unBase64 :: BS.ByteString }
  deriving(Eq, Ord, Read, Show)

instance FromJSON Base64 where
  parseJSON (String t) = either fail (return . Base64) . B64.decode . encodeUtf8 $ t
  parseJSON x          = fail ("expected String, got " ++ show x)

instance ToJSON Base64 where
  -- decodeUtf8 is safe, because B64.encode should only ever output ASCII characters
  -- (which makes a valid Unicode string)
  toJSON = String . decodeUtf8 . B64.encode . unBase64

------------------ Utilities ------------------

-- Utility to parse UTF-8 'Text' as JSON-encoded data
parseUtf8Text :: FromJSON a => T.Text -> Parser a
parseUtf8Text = (>>= parseJSON)    -- Parser Value -> Parser a
              . either fail return -- Either Error Value -> Parser Value
              . parseOnly value    -- ByteString -> Either Error Value
              . encodeUtf8         -- Text to ByteString

