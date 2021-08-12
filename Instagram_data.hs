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

-- data type Artists
data Artist = Artist ArtistName Gender Followers Date [YearlyFollowers] deriving (Eq, Show, Read)

-- add new Artist -- function 1
addArtist :: ArtistName -> Gender -> Followers -> Date -> [Artist] -> [Artist]
addArtist name gender followers date artists = (Artist name gender followers date []) : artists

-- display artists as string -- function 2
artistString :: Artist -> String
artistString all@(Artist name gender followers date yearly)
    = "Name: " ++ name ++ ", Gender: " ++ gender ++ ", Followers: " ++ show followers ++ ", Date: " ++ date ++ "\n" ++ "\n"
-- shorter version of artist string -- function 2
artistShortString :: Artist -> String
artistShortString all@(Artist name gender followers date yearly)
    = "Name: " ++ name ++ ", Followers: " ++ show followers ++  "\n" ++ "\n"

-- display all Artists as list -- function 2
artListStr :: [Artist] -> String
artListStr = foldr (++) "\n" . map artistString

-- display all Artists as short string list -- function 2
artListShortStr :: [Artist] -> String
artListShortStr = foldr (++) "\n" . map artistShortString

-- get name function for database duplicate comparison -- function 1 helper
getName :: Artist -> ArtistName
getName (Artist name _ _ _ _) = name

-- get date to show artist last updated on a certain date -- function 3 helper
getDate :: Artist -> String
getDate (Artist _ _ _ date _) = date

-- get number of followers
getFollowers :: Artist -> Followers
getFollowers (Artist _ _ followers _ _) = followers

-- filters artist by given year followers were updated
testDates :: [Artist] -> [Artist]
testDates [] = []
testDates artistList=filter((=="2017") . getDate) artistList 

artistToString :: Artist -> String
artistToString (Artist name gender followers date yearly) = ("Name: " ++ name ++ "\nGender: " ++ gender ++ "\nFollowers: " ++ (show followers) ++ "\nDate Updated: " ++ date ++ "\n")

-- DEMO FUNCTIONS --

-- display all command
displayAllArtist :: Int -> IO()
displayAllArtist 0 = do
   putStrLn ""
   putStrLn ("********** Below Are All Artist Inside The Database.**********")
   putStrLn ""
   putStrLn $ artListShortStr testDatabase
   
-- add artist demo
addArtistTest 1 = do
  let name = "John Smith"
  let gender = "Male"
  let followers = 9100
  let date = "21-05-2021"
  putStrLn ""
  putStrLn ("***** A new artist - " ++ name ++ " - has been saved to the database *****")
  putStrLn ""
  putStrLn $ artListStr $ addArtist name gender followers date testDatabase

-- Function 3 filter artist by date 
allArtistOnDate :: [Artist] -> String -> String
allArtistOnDate [] targetYear = ""
allArtistOnDate ((Artist name gender followers date yearly):xs) targetYear
    | date == targetYear = ("Name: " ++ name ++
                        "\nGender: " ++ gender ++
                        "\nFollowers: " ++ (show followers) ++
                        "\nDate: " ++ date ++ "\n") 
                        ++ "\n \n" ++ allArtistOnDate xs targetYear
    | otherwise = allArtistOnDate xs targetYear


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
      putStrLn $ artListShortStr ls
      putStrLn ""
      menuList ls

     -- Show artist updated on specific date
    | option == "3" = do
      putStrLn "Please enter the date you wish to search"
      menuList ls

     -- Show end-of-year numbers for a specific artist
    | option == "4" = do
      putStrLn "Pending Completion" 

     -- Current average number of followers across all artist
    | option == "5" = do
      putStrLn "Pending Completion"
      menuList ls

     -- display artist with more followers than specified input
    | option == "6" = do
      putStrLn "Pending Completion" 
      menuList ls 

     -- Updates end-of-year numbers on a specified artist
    | option == "7" = do
      putStrLn "Pending Completion"
      menuList ls

     -- display artist that have averages followers between two specified numbers
    | option == "8" = do
      putStrLn "Pending Completion"
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
