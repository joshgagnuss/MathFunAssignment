import Data.Set (toList, fromList)
import Data.List (delete, intercalate, nub, genericLength, sortOn, sort,sortBy, isInfixOf)
import qualified Data.List as L
import Text.Printf
import System.IO
import Data.Function
import Control.Applicative
import Data.Maybe (isJust, fromJust)

-- Type Class Artists
type InstaId = String
type ArtistName = String
type Gender = String
type Followers = Int
type Date = String
type YearlyFollowers = (String, Int)
type EndYear = [YearlyFollowers]

-- data type Artists
data Artist = Artist ArtistName Gender Followers Date [YearlyFollowers] deriving (Eq, Show, Read)

-- helper functions
-- used on conjunction with IO functions

addArtist :: String -> String -> Int -> String -> [Artist] -> [Artist]
addArtist name gender followers date db = db ++ [Artist name gender followers date []]

-- Produces list of artists as string
artistAsString :: [Artist] -> String
artistAsString db = foldr (++) [] (map formatArtistAsString db)

-- formats Artist as a string
formatArtistAsString :: Artist -> String
formatArtistAsString (Artist name gender followers date yearly ) = name ++ ", " ++ gender ++ ", " ++ show followers ++ ", " ++ date ++"\n"

-- filters artist by date entered
listArtistByDate :: String ->[Artist] -> [Artist]
listArtistByDate date db = filter (\(Artist _ _ _ date _) -> date == date) db

-- converts listArtistByDate to string
listArtistByDateString :: String -> [Artist] -> String
listArtistByDateString date db = artistAsString (listArtistByDate date db)

calcAvg :: Artist -> Float
calcAvg (Artist _ _ _ _ yearly) = calcAvg1 yearly

calcAvg1 :: [YearlyFollowers] -> Float
calcAvg1 [] = 0
calcAvg1 yearly = fromIntegral (sum (map snd yearly)) / fromIntegral (length yearly)

-- calculates the average followers of the artist list
averageFollowersOfArtist :: [Artist] -> Float
averageFollowersOfArtist [] = 0
averageFollowersOfArtist db = (sum (map calcAvg db)) / fromIntegral (length db)

-- checks for records of yearly numbers are present 
artistYearlyExists :: String -> EndYear -> Bool
artistYearlyExists year followers
    | (filter (\(a,_) -> a == year) followers) == [] = False
    | otherwise = True

-- shows artists with yearly numbers as string
artistWithYearlyAsString :: String -> Artist -> String
artistWithYearlyAsString year (Artist name _ _ _ yearly)
    | artistYearlyExists year yearly = name ++ ", " ++ show (snd (head [x | x <- yearly, fst x == year])) ++ "\n"
    | otherwise = ""

-- checks if artist exists in the database 
artistExists :: String -> [Artist] -> Bool
artistExists name db
    | (filter (\(Artist name _ _ _ _) -> name == name) db) == [] = False
    | otherwise = True

-- filters artist by name 
artistByName :: String -> [Artist] -> Artist
artistByName name db = head (filter (\(Artist name _ _ _ _) -> name == name) db)

-- creates new end of year numbers for artist
newYearly :: Artist -> String -> Int -> Artist
newYearly (Artist name gender followers date yearly) year yrfollowers = (Artist name gender followers date ((filter (\(a,b) -> a /= year) yearly) ++ [(year, yrfollowers)]))

-- adds new end of year numbers for the artist to datatbase
addYearlyNumbers :: String -> String -> Int -> [Artist] -> [Artist]
addYearlyNumbers name year followers db
    | not (artistExists name db) = db
    | otherwise = (filter (\(Artist name _ _ _ _) -> name /= name) db) ++ [newYearly (artistByName name db) year followers]

-- checks database to match artist with given date
dateExists :: String -> [Artist] -> Bool
dateExists date db
    | (filter (\(Artist _ _ _ date _) -> date == date) db) == [] = False
    | otherwise = True

-- takes list sorted by date and lists as string
sortedYearListAsString :: String -> String -> [Artist] -> String
sortedYearListAsString dateB dateE db = artistAsString (sortFilmsByRating (listArtistByYears dateB dateE db))

-- filters artist by year entered 
listArtistByYears :: String -> String -> [Artist] -> [Artist]
listArtistByYears dateB dateE db = filter (\(Artist _ _ _ date _) -> date >= dateB && date <= dateE) db

-- yet modify this function to suit 
sortFilmsByRating :: [Artist] ->[Artist]
sortFilmsByRating db = reverse (map fst (sortBy (compare `on` snd) (map getRating db)))

-- used to pass back a tuple used to sort films by rating
getRating :: Artist -> (Artist, Float)
getRating artist = (artist, calcAvg artist)


-- demo functions to display each function working using testDatabase
demo :: Int -> IO ()
-- demo 1 will add an artist to the testDatabase and then list the database with new artist included 
demo 1 = putStrLn (artistAsString (addArtist "John Smith" "Male" 31000 "2021-02-18" testDatabase))




-- Test Data
testDatabase :: [Artist]
testDatabase = 
  [
    Artist "Zoe Tay" "Female" 30200 "2017" [("2020", 30000), ("2019", 28000), ("2018", 24000), ("2017", 15000)],

     Artist "Huang Biren" "Female" 9121 "2021-02-18" [("2020", 9100), ("2019", 8888), ("2018", 6000)],

     Artist "Xiang Yun" "Female" 27000 "2021-02-18" [("2020", 25000), ("2019", 20000), ("2018", 4000), ("2017", 500)],

     Artist "Dawn Yeoh" "Female" 33600 "2021-02-18" [("2020", 33000), ("2019", 20000), ("2018", 8000), ("2017", 600)],
     
     Artist "Kym Ng" "Female" 57800 "2021-02-18" [("2020", 55000), ("2019", 46000), ("2018", 20000), ("2017", 15000)],
     
     Artist "Carrie Wong" "Female" 58300 "2021-02-18" [("2020", 55000), ("2019", 47000), ("2018", 19000), ("2017", 18000)],
     
     Artist "Vivian Lai" "Female" 59200 "2021-02-18" [("2020", 59000), ("2019", 57000), ("2018", 49000), ("2017", 48000)],
     
     Artist "Michelle Chong" "Female" 59800 "2021-02-18" [("2020", 58800), ("2019", 59000), ("2018", 49000), ("2017", 38000)],
     
     Artist "Priscelia Chan" "Female" 65400 "2021-02-18" [("2020", 65000),("2019", 58800), ("2018", 57000), ("2017", 49000), ("2016", 38000)],
     
     Artist "Felicia Chin" "Female" 71700 "2021-02-18" [("2020", 69000),("2019", 68800), ("2018", 59000), ("2017", 48000), ("2016", 39000)],
     
     Artist "Ya Hui" "Female" 92200 "2021-02-18" [("2020", 85000),("2019", 78800), ("2018", 69000), ("2017", 64000), ("2016", 58000), ("2015",5000)],
     
     Artist "Joanne Peh" "Female" 10400 "2021-02-18" [("2020", 9400), ("2019", 8000)],
     
     Artist "Jesseca Liu" "Female" 106000 "2021-02-02" [("2020", 95000), ("2019", 88000), ("2018", 76000), ("2017", 10000)],
     
     Artist "Julie Tan" "Female" 151000 "2021-02-18" [("2020", 141000), ("2019", 90000), ("2018", 80000), ("2017", 60000)],
     
     Artist "Jeanette Aw" "Female" 159000 "2021-02-18" [("2020", 149000), ("2019", 110000), ("2018", 98000), ("2017", 90000)]
  ]
