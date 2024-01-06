{-#  LANGUAGE DerivingStrategies #-}
{-#  LANGUAGE ImportQualifiedPost #-}
module Main where
import Data.Aeson
import Data.Aeson.Types
import Data.Map.Strict 
import Data.Text 
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TL
import Data.Text.Lazy.Builder.RealFloat qualified as TL
import Data.ByteString.Lazy 
import Data.Time
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)

-- https://stackoverflow.com/questions/55079309/aeson-encoding-of-data-map-strict-map-with-custom-key-type-results-in-array-of-a
data FunnyKey = FunnyKey String Rational Day deriving stock (Eq, Ord, Show)

renderTheRatio :: Rational -> Text
renderTheRatio r = 
    let f = (fromRational r) :: Double
     in TL.toStrict $ TL.toLazyText $ TL.realFloat f

-- This still needs more work.
renderTheDay :: Day -> Text
renderTheDay d = T.pack (show d) 

--
--
funnyKeyToText :: FunnyKey -> Text
funnyKeyToText (FunnyKey i r d) = 
    T.pack i <> 
    T.pack "_" <> 
    renderTheRatio r <> 
    T.pack "_" <> 
    renderTheDay d

instance ToJSON FunnyKey where
    toJSON funnyKey = String $ funnyKeyToText funnyKey

instance ToJSONKey FunnyKey where
    toJSONKey = toJSONKeyText funnyKeyToText

funnyKeyFromText :: Text -> Parser FunnyKey
funnyKeyFromText someText = case T.split (=='_') someText of 
    -- we expect to find three components separated by '_'
    [theName, theRatio, theDay] -> 
        FunnyKey <$> pure (T.unpack theName) <*> parseTheRatio theRatio <*> parseTheDay theDay
    _ -> fail "Unexpected structure"

parseTheRatio :: Text -> Parser Rational
parseTheRatio t = 
    case T.rational t of
        Right (r, rest) | T.length rest == 0 -> pure r
        _ -> fail "could ot parse rational"

parseTheDay :: Text -> Parser Day
parseTheDay = undefined

instance FromJSON FunnyKey where
    parseJSON v = case v of
        String someText -> funnyKeyFromText someText 
        _ -> fail "Unexpected non-String value"

instance FromJSONKey FunnyKey where
  fromJSONKey = FromJSONKeyTextParser funnyKeyFromText
--
--

funnyMap :: Map FunnyKey Int
funnyMap = Data.Map.Strict.fromList [
    (FunnyKey "aa" 1.0 (fromOrdinalDate 2024 1), 100),
    (FunnyKey "bb" 2.5 (fromOrdinalDate 2024 1), 101),
    (FunnyKey "cc" 3.0 (fromOrdinalDate 2024 1), 103)
    ]

main :: IO ()
main = do
    Data.ByteString.Lazy.putStr $ encode funnyMap
    -- output looks like: {"aa_1.0_2024-01-01":100,"bb_2.5_2024-01-01":101,"cc_3.0_2024-01-01":103}