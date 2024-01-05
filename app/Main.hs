{-#  LANGUAGE DerivingStrategies #-}
{-#  LANGUAGE ImportQualifiedPost #-}
module Main where
import Data.Aeson
import Data.Aeson.Types
import Data.Map.Strict 
import Data.Text 
import Data.Text qualified as T
import Data.ByteString.Lazy 

-- https://stackoverflow.com/questions/55079309/aeson-encoding-of-data-map-strict-map-with-custom-key-type-results-in-array-of-a
data FunnyKey = FunnyKey Int Bool deriving stock (Eq, Ord, Show)

funnyKeyToText :: FunnyKey -> Text
funnyKeyToText (FunnyKey i b) = T.pack (show i) <> T.pack "_" <> T.pack (show b)

instance ToJSON FunnyKey where
    toJSON funnyKey = String $ funnyKeyToText funnyKey

instance ToJSONKey FunnyKey where
    toJSONKey = toJSONKeyText funnyKeyToText


funnyMap :: Map FunnyKey Int
funnyMap = Data.Map.Strict.fromList [
    (FunnyKey 1 False, 100),
    (FunnyKey 2 True, 101),
    (FunnyKey 3 False, 103)
    ]

main :: IO ()
main = do
    Data.ByteString.Lazy.putStr $ encode funnyMap
    -- output should be: {"1_False":100,"2_True":101,"3_False":103}