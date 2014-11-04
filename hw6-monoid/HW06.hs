{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module HW06 where
import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List

ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Object o)   = Object $ fmap ynToBool o
ynToBool (Array a)    = Array  $ fmap ynToBool a
ynToBool v            = v

parseData :: B.ByteString -> Either String Value
parseData = fmap ynToBool . eitherDecode


data Person = Person 
  { name :: String, age :: Int }
  deriving (Show, Generic)

instance FromJSON Person

p :: Either String Person
p = eitherDecode "{ \"name\":\"Richard\", \"age\": 32}"


data Market = Market {
  fmid :: Int,
  marketname :: T.Text,
  website :: T.Text,
  x :: Float,
  y :: Float,
  credit :: Bool,
  wic :: Bool,
  wiccash :: Bool,
  sfmnp :: Bool,
  snap :: Bool,
  bakedgoods :: Bool,
  cheese :: Bool,
  crafts :: Bool,
  flowers :: Bool,
  eggs :: Bool,
  seafood :: Bool,
  herbs :: Bool,
  vegetables :: Bool,
  honey :: Bool,
  jams :: Bool,
  maple :: Bool,
  meat :: Bool,
  nursery :: Bool,
  nuts :: Bool,
  plants :: Bool,
  poultry :: Bool,
  prepared :: Bool,
  soap :: Bool,
  trees :: Bool,
  wine :: Bool
} deriving (Show, Generic, Eq)

instance FromJSON Market


loadData :: IO [Market]
loadData = do
  filedata <- B.readFile "markets.json"
  case parseData filedata of
    Left err -> fail err
    Right v -> do
      case fromJSON v of
        Error err -> fail err
        Success a -> return a


data OrdList a = OrdList { 
  getOrdList :: [a] 
} deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
  mempty                            = OrdList []
  mappend (OrdList as) (OrdList bs) = OrdList $ sort $ as ++ bs

-- you’ll need to fill in the ellipses

evens :: OrdList Integer
evens = OrdList [2,4,6]
odds :: OrdList Integer
odds = OrdList [1,3,5]
combined :: OrdList Integer
combined = evens <> odds


type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search toMonoid txt = foldr combine mempty where
  isMatch market = txt `T.isInfixOf` (marketname market)
  combine market m
    | isMatch market = toMonoid market <> m
    | otherwise = m

compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.)



firstFound :: Searcher (Maybe Market)
firstFound = compose2 getFirst $ search (First . Just) 

allFound :: Searcher [Market]
allFound = search (:[])

numberFound :: Searcher Int
numberFound = compose2 getSum $ search one where
  one _ = (Sum 1)

data MarketY = MarketY {
  getMarketY :: Market
} deriving (Eq, Show)

instance Ord MarketY where
  (MarketY m1) `compare` (MarketY m2) = (y m1) `compare` (y m2)

orderedNtoS :: Searcher [Market]
orderedNtoS = compose2 (map getMarketY) $ compose2 getOrdList $ search (OrdList . (:[]) . MarketY)

--data Firsty a = Firsty (Maybe a) deriving (Eq, Show)

--getFirsty :: Firsty a -> Maybe a 
--getFirsty (Firsty x) = x

--instance Monoid (Firsty a) where
  --mempty = Firsty Nothing

  --mappend (Firsty Nothing) b  = b
  --mappend a (Firsty Nothing)  = a
  --mappend (Firsty (Just a)) _ = Firsty (Just a)

--{
  --"fmid":1005969,
  --"marketname":"\"Y Not Wednesday Farmers Market at Town Center\"",
  --"website":"http://www.sandlercenter.org/index/ynotwednesdays",
  --"street":"201 Market Street,",
  --"city":"Virginia Beach",
  --"county":"Virginia Beach",
  --"state":"Virginia",
  --"zip":"23462",
  --"season1date":"June to August",
  --"season1time":"Wed:5:00 PM - 8:00 PM;",
  --"season2date":"",
  --"season2time":"",
  --"season3date":"",
  --"season3time":"",
  --"season4date":"",
  --"season4time":"",
  --"x":-76.135361,
  --"y":36.841885,
  --"location":"Other",
  --"credit":"Y",
  --"wic":"N",
  --"wiccash":"N",
  --"sfmnp":"N",
  --"snap":"N",
  --"bakedgoods":"Y",
  --"cheese":"Y",
  --"crafts":"N",
  --"flowers":"Y",
  --"eggs":"Y",
  --"seafood":"Y",
  --"herbs":"N",
  --"vegetables":"Y",
  --"honey":"Y",
  --"jams":"Y",
  --"maple":"N",
  --"meat":"N",
  --"nursery":"N",
  --"nuts":"N",
  --"plants":"N",
  --"poultry":"N",
  --"prepared":"Y",
  --"soap":"Y",
  --"trees":"N",
  --"wine":"Y",
  --"updatetime":"5/5/12 17:56"
--},

















