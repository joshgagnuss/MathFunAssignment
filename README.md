# MathFunAssignment
Haskell Database Sofwtare

simple haskell databse software to manage data of local artists instagram accounts

open the program


there is some demo functions you can use to interact with the test database below

simply type demo 1 or demo 2 ... and so on to see the result

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

typing "main" will present you with the user interface where you can interact with the txt file database


 "****************************************"
"******************Menu******************"
 "1. Add New Artist to Database"
 "2. Display All Artist Saved to Database"
 "3. Show Artists Updated On A Specified Date (yyyy-mm-dd)"
 "4. Show Artist's with end-of-year data for 2017"
 "5. Current Average Number Of Followers Of All Artists"
 "6. Display Artist With More Followers Than Specified Number"
 "7. Update End-Of-Year Numbers Of A Specified Artist"
 "8. Display Artists Averages Between Two Numbers"
 "0. Exit Program"
 "****************************************"
 "****************************************"
  
select and option by the number associated with it and you can interact with the database

