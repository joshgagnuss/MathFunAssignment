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

-- Functional Helpers 
addArtist :: String -> String -> Int -> String [Artist] -> [Artist]
addArtist name gender followers date db = db ++ [Artist name gender followers date []]

-- Produces list of artists as string
artistAsString :: [Artist] -> String
artistAsString db = foldr (++) [] (map formatArtistAsString db)

-- formats Artist as a string
formatArtistAsString :: Artist -> String
formatArtistAsString (Artist namae gender followers date yearly ) = name ++ ", " ++ gender ++ ", " ++ show followers ++ ", " ++ date ++"\n"

-- filters artist by date entered
listArtistByDate :: String ->[Artist] -> [Artist]
listArtistByDate date db = filter (\(Artist _ _ _ date _) -> date == date) db

-- converts listArtistByDate to string
listArtistByDateString :: String -> [Artist] -> String
listArtistByDateString date db = artistAsString (listArtistByDate date db)

--**************************************
averageRatingofList :: [Artist] -> Float
averageRatingofList [] = 0
averageRatingofList db = (sum (map calcFilmRating db)) / fromIntegral (length db)

userRatingsAsString :: String -> [Film] -> String
userRatingsAsString user db = foldr (++) [] (map (\film -> userRatingOfFilmAsString user film) db)

userRatingOfFilmAsString :: String -> Film -> String
userRatingOfFilmAsString user (Film title _ _ ratings)
    | userRatingExists user ratings = title ++ ", " ++ show (snd (head [x | x <- ratings, fst x == user])) ++ "\n"
    | otherwise = ""

userRatingExists :: String -> Ratings -> Bool
userRatingExists user ratings
    | (filter (\(a,_) -> a == user) ratings) == [] = False
    | otherwise = True

addUserRating :: String -> String -> Int -> [Film] -> [Film]
addUserRating title user review db
    | not (filmExists title db) = db
    | otherwise = (filter (\(Film ftitle _ _ _) -> ftitle /= title) db) ++ [newRating (filmByTitle title db) user review]

newRating :: Film -> String -> Int -> Film
newRating (Film ftitle fdir fyr ratings) user review = (Film ftitle fdir fyr ((filter (\(a,b) -> a /= user) ratings) ++ [(user, review)]))

filmByTitle :: String -> [Film] -> Film
filmByTitle title db = head (filter (\(Film ftitle _ _ _) -> ftitle == title) db)

filmExists :: String -> [Film] -> Bool
filmExists title db
    | (filter (\(Film ftitle _ _ _) -> ftitle == title) db) == [] = False
    | otherwise = True

directorExists :: String -> [Film] -> Bool
directorExists director db
    | (filter (\(Film _ fdirector _ _) -> fdirector == director) db) == [] = False
    | otherwise = True

sortedYearListAsString :: Int -> Int -> [Film] -> String
sortedYearListAsString yrB yrE db = filmsAsString (sortFilmsByRating (listFilmsByYears yrB yrE db))

sortFilmsByRating :: [Film] ->[Film]
sortFilmsByRating db = reverse (map fst (sortBy (compare `on` snd) (map getRating db)))

-- used to pass back a tuple used to sort films by rating
getRating :: Film -> (Film, Float)
getRating film = (film, calcFilmRating film)

listFilmsByYears :: Int -> Int -> [Film] -> [Film]
listFilmsByYears yrB yrE db = filter (\(Film _ _ yr _) -> yr >= yrB && yr <= yrE) db





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
