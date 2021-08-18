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

-- adds new artist to database
addArtist :: String -> String -> Int -> String -> [Artist] -> [Artist]
addArtist name gender followers date db = db ++ [Artist name gender followers date []]

-- Produces list of artists as string
artistAsString :: [Artist] -> String
artistAsString db = foldr (++) [] (map formatArtistAsString db)

-- helper funtion to check for artists name
getName :: Artist -> ArtistName
getName (Artist name _ _ _ _) = name

-- formats Artist as a string
formatArtistAsString :: Artist -> String
formatArtistAsString (Artist name gender followers date yearly ) = "Name: " ++ name ++ ", Gender: " ++ gender ++ ", Followers: " ++ show followers ++ ", Date Updated: " ++ date ++"\n"

-- filters artist by date entered
listArtistByDate :: String ->[Artist] -> [Artist]
listArtistByDate date db = filter (\(Artist _ _ _ dt _) -> dt == date) db

-- converts listArtistByDate to string
listArtistByDateString :: String -> [Artist] -> String
listArtistByDateString date db = artistAsString (listArtistByDate date db)

-- calculates average following
calcAvg :: Artist -> Float
calcAvg (Artist _ _ _ _ yearly) = calcAvg1 yearly

-- calculates the average following of artist
calcAvg1 :: [YearlyFollowers] -> Float
calcAvg1 [] = 0
calcAvg1 yearly = fromIntegral (sum (map snd yearly)) / fromIntegral (length yearly)

-- calculates average followers of individual artists 
getAvgFollowers :: [YearlyFollowers] -> Float
getAvgFollowers [] = 0
getAvgFollowers fl = (realToFrac (sum (map snd fl)) / fromIntegral (length fl)) :: Float

-- gets the average following of artist and prints to string
artistAvgStr :: Artist -> String
artistAvgStr all@(Artist name _ _ _ yearly)
   = "Name: " ++ name ++ ", Average Followers: " ++ printf"%.2g \n"(getAvgFollowers yearly)

-- maps filtered averages to string
avgAsString :: [Artist] -> String
avgAsString db = foldr (++) [] (map artistAvgStr db)

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

-- prints the artist and yearly as a string
yearlyAsString :: String -> [Artist] -> String
yearlyAsString year db = foldr (++) [] (map (\artist -> artistWithYearlyAsString year artist) db)

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

-- filters artist by year entered 
listArtistByYears :: String -> String -> [Artist] -> [Artist]
listArtistByYears dateB dateE db = filter (\(Artist _ _ _ date _) -> date >= dateB && date <= dateE) db

--filters artist above a number entered
listArtistByFollowers :: Float -> [Artist] -> [Artist]
listArtistByFollowers f db = filter (\(Artist _ _ _ _ yearly) -> calcAvg1 yearly >= f) db

-- filters artist for demo function 6
artistAbove1000AsString :: Float -> [Artist] -> String
artistAbove1000AsString f db = artistByFollowingAsString f db

-- list artist filtered by followersrs
artistByFollowingAsString :: Float -> [Artist] -> String
artistByFollowingAsString f db = artistAsString (listArtistByFollowers f db)

-- helper function to return a tuple to sort in descending order
getAverage :: Artist -> (Artist, Float)
getAverage artist = (artist, calcAvg artist)

--sorts a list of artist by average
sortArtistByAverage :: [Artist] -> [Artist]
sortArtistByAverage db = reverse (map fst (sortBy (compare `on` snd) (map getAverage db)))

--filters artist above a number entered
listArtistByAverage :: Float -> Float -> [Artist] -> [Artist]
listArtistByAverage fa fb db = filter (\(Artist _ _ _ _ yearly) -> calcAvg1 yearly >= fa && calcAvg1 yearly <= fb) db

-- maps sorted by average to string
sortedByAverageString :: Float -> Float -> [Artist] -> String
sortedByAverageString fa fb db = avgAsString (sortArtistByAverage (listArtistByAverage fa fb db))

-- demo functions to display each function working using testDatabase
demo :: Int -> IO ()
-- demo 1 will add an artist to the testDatabase and then list the database with new artist included 
demo 1 = putStrLn (artistAsString (addArtist "John Smith" "Male" 31000 "2021-02-18" testDatabase))
-- demo 2 will list all the artist inside the database
demo 2 = putStrLn (artistAsString testDatabase)
-- demo 3 will show all artists updated on a certain date
demo 3 = putStrLn (listArtistByDateString "2021-02-18" testDatabase)
-- demo 4 shows all artist who have recorded end of year numbers for 2017
demo 4 = putStrLn (yearlyAsString "2017" testDatabase)
-- demo 5 will give the average number of followers based on their last recorded numbers
demo 5 = putStrLn (avgAsString testDatabase)
-- demo 6 gives the name of the artists that have more followers then a specified number of followers in a certain year
demo 6 = putStrLn (artistAbove1000AsString 60000 testDatabase)
-- demo 7 allows the administrator to record the end of year numbers of a certain artist
demo 7 = putStrLn (artistAsString(addYearlyNumbers "Huang Biren" "2017" 35000 testDatabase))
-- demo 8 will give all local artists who has an average following between two given values(inclusive), sorted in descending order on followers
demo 8 = putStrLn (sortedByAverageString 30000 60000 testDatabase)


-- Main Program Interface --
dataFile = "data.txt"
main :: IO()
main = do
  dtf <- readFile dataFile
  menuList (read dtf :: [Artist])
menuList :: [Artist] -> IO ()
menuList artList = do
  putStrLn ""
  putStrLn ""
  putStrLn "****************************************"
  putStrLn "******************Menu******************"
  putStrLn ""
  putStrLn "1. Add New Artist to Database"
  putStrLn "2. Display All Artist Saved to Database"
  putStrLn "3. Show Artists Updated On A Specified Date (yyyy-mm-dd)"
  putStrLn "4. Show Artist's with end-of-year data for 2017"
  putStrLn "5. Current Average Number Of Followers Of All Artists"
  putStrLn "6. Display Artist With More Followers Than Specified Number"
  putStrLn "7. Update End-Of-Year Numbers Of A Specified Artist"
  putStrLn "8. Display Artists Averages Between Two Numbers"
  putStrLn "0. Exit Program"
  putStrLn ""
  putStrLn "****************************************"
  putStrLn "****************************************"
  putStrLn ""
  putStrLn " Please Select A Menu Option: "
  putStrLn ""
  option <- getLine
  putStrLn ""
  try artList option where
   try ls option
     -- Add new artist
    | option == "1" = do
      putStr "Artist Name: "
      name <- getLine
      putStr "Gender: "
      gender <- getLine
      putStr "Followers: "
      followers <- getLine
      putStr "Date: "
      date <- getLine
     -- check for duplicates
      if (filter ((==name) . getName) ls) == []
       then do
        putStrLn ("Artist:  " ++ name ++ "  has been saved to the database")
        menuList (addArtist name gender (read followers :: Int) date ls)
       else do
        putStrLn ("\nArtist: " ++ name ++ " already exists in the database, please enter another name")
        menuList ls

     -- Display all artist in database 
    | option == "2" = do
      putStrLn ("\n**** Below are all artist currently listed in the database ****")
      putStrLn ""
      putStrLn $ artistAsString ls
      putStrLn ""
      menuList ls

     -- Show artist updated on specific date
    | option == "3" = do
      putStrLn "Please enter the date you wish to search (yyyy-mm-dd): "
      date <- getLine
      putStrLn ""
      putStrLn (listArtistByDateString date ls)
      putStrLn ""
      menuList ls

     -- Show end-of-year numbers for a specific artist
    | option == "4" = do
      putStrLn (yearlyAsString "2017" ls) 
      menuList ls
     -- Current average number of followers across all artist
    | option == "5" = do
      putStrLn (avgAsString ls)
      menuList ls

     -- display artist with more followers than specified input
    | option == "6" = do
      putStrLn "please enter the number of followers you would like to filter: "
      f <- getLine
      putStrLn ""
      putStrLn (artistAbove1000AsString (read f :: Float) ls)
      menuList ls 

     -- Updates end-of-year numbers on a specified artist
    | option == "7" = do
      putStrLn "Please enter the artist you wish to update: "
      name <- getLine
      putStrLn "Please enter the year you would to add: "
      year <- getLine
      if (artistExists name ls)
            then do
              putStr "Enter the number of followers the artist has: "
              followers <- getLine
              putStr ""
              putStrLn ("You saved " ++ name ++ " yearly following number " ++ show(followers))
              menuList (addYearlyNumbers name year (read followers :: Int) ls)
            else do
             putStr "Artist does not exist in database"
             menuList ls

     -- display artist that have averages followers between two specified numbers
    | option == "8" = do
      putStrLn "Please enter the lower boundary of the filter: "
      fa <- getLine
      putStrLn "Please enter the upper boundary of the filter: "
      fb <- getLine
      if (read fa :: Float) >= 0 && (read fb :: Float) >= 0
            then do
               if (read fa :: Float) <= (read fb :: Float)
                    then  do
                       if (sortedByAverageString (read fa :: Float) (read fb :: Float) ls) /= ""
                            then do
                             putStr "\n List of artists with averages between: "
                             putStrLn (show(fa) ++ " and " ++ show(fb) ++ "\n")
                             putStr (sortedByAverageString (read fa :: Float) (read fb :: Float) ls)
                             putStrLn ""
                             putStrLn ""
                            else do
                             putStrLn "No artists have averages between these numbers"
                    else do
                     putStrLn "Upper boundary must be greater then lower boundary"
                     menuList ls
            else do
                 putStrLn "Numbers were invalid"
                 menuList ls

     -- Exit Program and save data 
    | option == "0" = do
      -- close the data.tzt
      seq ls (return ())
      -- write new data into data.txt
      writeFile dataFile (show ls)
      putStrLn "All data has been saved to database"
      putStrLn ""
      putStrLn "Bye Bye!"
      putStrLn ""

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
