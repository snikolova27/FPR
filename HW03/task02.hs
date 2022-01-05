import Data.Char
main :: IO()
main = do

    -- 1 minute == 60 seconds
    -- 1 hour = 3600 seconds
    -- 1 day == 86400 seconds
    -- 1 year of 365 days == 31556926 seconds


    print $ formatDuration 0 == "now"

    --one of each
    print $ formatDuration 1 == "1 second"
    print $ formatDuration 60 == "1 minute"
    print $ formatDuration 120 == "2 minutes"
    print $ formatDuration 3600 == "1 hour"
    print $ formatDuration 86400 == "1 day"
    print $ formatDuration 31556926 == "1 year"

    -- combinations of two - 10 of them

         --minute and seconds
    print $ formatDuration 62 ==  "1 minute and 2 seconds"

         --hour and seconds
    print $ formatDuration 3615  == "1 hour and 15 seconds"

         -- days and seconds
    print $ formatDuration 172803 == "2 days and 3 seconds"

        -- year and seconds
    print $ formatDuration 31556932 == "1 year and 6 seconds"

        -- hour and minutes
    print $ formatDuration 3660 == "1 hour and 1 minute"

        -- days and minutes
    print $ formatDuration 86520  == "1 day and 2 minutes"

        --days and hours
    print $ formatDuration 176400  == "2 days and 1 hour"

        --year with minutes
    print $ formatDuration 31556978 == "1 year and 52 seconds"

        -- year with hours
    print $ formatDuration 31564126  == "1 year and 2 hours"

        -- year with days
    print $ formatDuration 31729726  == "1 year and 2 days"


    -- combiantions of three - 10 of them

      -- hours, minutes and seconds
    print $ formatDuration 3662 == "1 hour, 1 minute and 2 seconds"
    print $ formatDuration 3785 == "1 hour, 3 minutes and 5 seconds"

      -- days, minutes and seconds
    print $ formatDuration 86614 == "1 day, 3 minutes and 34 seconds"

      -- year, minutes and seconds
    print $ formatDuration 31557170  == "1 year, 4 minutes and 4 seconds"

      -- days, hours and seconds
    print $ formatDuration  183624 == "2 days, 3 hours and 24 seconds"

      -- year, hours and seconds
    print $ formatDuration 31564173  == "1 year, 2 hours and 47 seconds"

      -- year, days and seconds
    print $ formatDuration 31643327 == "1 year, 1 day and 1 second"

        -- year, days and hours
    print $ formatDuration 31736926  == "1 year, 2 days and 2 hours"
        -- year, days and minutes
    print $ formatDuration 31729966  == "1 year, 2 days and 4 minutes"
        -- year, hours and minutes
    print $ formatDuration 31569406 == "1 year, 3 hours and 28 minutes"
        -- days, hours and minutes
    print $ formatDuration 880560  == "10 days, 4 hours and 36 minutes"

  
    --combinations of four - 5 of them 
    -- years, days, hours and minutes
    print $ formatDuration  31747966 == "1 year, 2 days, 5 hours and 4 minutes" 

    -- years, days, hours and seconds
    print $ formatDuration 31650527  == "1 year, 1 day, 2 hours and 1 second"

    -- years, hours, minutes and seconds
    print $ formatDuration 31569409 == "1 year, 3 hours, 28 minutes and 3 seconds"

    -- years, days, minutes and seconds 
    print $ formatDuration 31729986  == "1 year, 2 days, 4 minutes and 20 seconds"

     -- days, hours, minutes and seconds
    print $ formatDuration  184884 == "2 days, 3 hours, 21 minutes and 24 seconds"

    --combination of all five
    print $ formatDuration  31747996 == "1 year, 2 days, 5 hours, 4 minutes and 30 seconds" 


getYears :: Int -> Int 
getYears x  
 | x >= 31556926 = div x 31556926
 | otherwise = 0

getDays :: Int -> Int 
getDays x 
 | x >=  31556926 = div ( x - getYears x  *31556926) 86400 
 | otherwise = div x 86400 

timeWithoutYearsAndDays :: Int -> Int 
timeWithoutYearsAndDays x
 | x >= 31556926 = x - getYears x * 31556926 - getDays x * 86400
 | x < 31556926 && x>= 86400 = x - getDays x * 86400
 | otherwise = x

getHours :: Int -> Int 
getHours x  = div x 3600

getMinutes :: Int -> Int 
getMinutes x = div (mod x 3600 - mod x 60) 60 

getSeconds :: Int -> Int 
getSeconds x = mod x 60

getListOfTimes :: Int -> [Int]
getListOfTimes 0 = [0]
getListOfTimes x = [getYears x, getDays x, getHours onlyTimes, getMinutes onlyTimes, getSeconds onlyTimes]
 where onlyTimes = timeWithoutYearsAndDays x

onlyYears:: [Int] -> String 
onlyYears [] = ""
onlyYears (x:xs)
 | x == 0 = ""
 | x == 1 = "1 year"
 | otherwise = show x ++ " years"

onlyDays :: [Int] -> String 
onlyDays [] = ""
onlyDays [_] = ""
onlyDays (x:d:xs)
 | d == 0 = ""
 | d == 1 = "1 day"
 | otherwise = show d ++ " days"

onlyHours :: [Int] -> String 
onlyHours [] = ""
onlyHours [_] = ""
onlyHours [_,_] = ""
onlyHours (x:y:h:xs)
 | h == 0 = ""
 | h == 1 = "1 hour"
 | otherwise = show h ++ " hours"

onlyMins :: [Int] -> String
onlyMins [] = ""
onlyMins [_] = ""
onlyMins [_,_] = ""
onlyMins [_,_,_] = ""
onlyMins (x:y:h:m:xs)
 | m == 0 = ""
 | m == 1 = "1 minute"
 | otherwise  = show m ++ " minutes" 

onlySec :: [Int] -> String
onlySec [] = ""
onlySec [_] = ""
onlySec [_,_] = ""
onlySec [_,_,_] = ""
onlySec [_,_,_, _] = ""
onlySec (x:y:h:m:z:xs)
 | z == 0 = ""
 | z == 1 = "1 second"
 | otherwise  = show z ++ " seconds" 

anyYears :: [Int] -> Bool 
anyYears [] = False
anyYears (x:xs) = x /= 0

anyDays :: [Int] -> Bool 
anyDays [] = False
anyDays [_] = False
anyDays (x:d:xs) = d /= 0

anyHours :: [Int] -> Bool 
anyHours [] = False 
anyHours [_] = False 
anyHours [_,_] = False 
anyHours (x:y:h:xs) = h /= 0

anyMins :: [Int] -> Bool 
anyMins [] = False
anyMins [_] = False 
anyMins [_,_] = False 
anyMins [_,_,_] = False
anyMins (x:y:h:m:xs) = m /= 0

anySeconds :: [Int] -> Bool 
anySeconds [] = False
anySeconds [_] = False 
anySeconds [_,_] = False
anySeconds [_,_,_] = False
anySeconds [_,_,_, _] = False
anySeconds (x:y:h:m:s:xs) = s /= 0

getStringWithTime :: [Int] -> String 
getStringWithTime [] = ""
getStringWithTime xs
 | not (anyYears xs) && not (anyDays xs) && not (anyHours xs) && not (anyMins xs) && not (anySeconds xs) = "now"         -- given 0
 | not (anyYears xs) && not (anyDays xs) && anyHours xs && not (anyMins xs) && not (anySeconds xs) = onlyHours xs        -- only hours
 | not (anyYears xs) && not (anyDays xs) && not (anyHours xs) && anyMins xs && not (anySeconds xs) = onlyMins xs         -- only minutes
 | not (anyYears xs) && not (anyDays xs) && not (anyHours xs) && not (anyMins xs) && anySeconds xs = onlySec xs          -- only seconds
 | anyYears xs && not (anyDays xs) && not (anyHours xs) && not (anyMins xs) && not (anySeconds xs) = onlyYears xs        -- only years
 | not (anyYears xs) && anyDays xs && not (anyHours xs) && not (anyMins xs) && not (anySeconds xs) = onlyDays xs         -- only days
 | not (anyYears xs) && not (anyDays xs) && anyHours xs && anyMins xs && not (anySeconds xs) = onlyHours xs ++ " and " ++ onlyMins xs  -- hours + minutes
 | not (anyYears xs) && not (anyDays xs) && anyHours xs && not (anyMins xs) && anySeconds xs = onlyHours xs ++ " and " ++ onlySec xs   -- hours + seconds
 | not (anyYears xs) && not (anyDays xs) && not (anyHours xs) && anyMins xs && anySeconds xs = onlyMins xs ++ " and " ++ onlySec xs    -- minutes + seconds
 | not (anyYears xs) && not (anyDays xs) && anyHours xs && anyMins xs && anySeconds xs = onlyHours xs ++ ", " ++ onlyMins xs ++ " and " ++ onlySec xs   -- hours + minutes + seconds
 | not (anyYears xs) && anyDays xs && anyHours xs && anyMins xs && anySeconds xs = onlyDays xs ++ ", " ++ onlyHours xs ++ ", " ++ onlyMins xs ++ " and " ++ onlySec xs -- days + hours + minutes + seconds
 | anyYears xs &&  not (anyDays xs) && anyHours xs && anyMins xs && anySeconds xs = onlyYears xs ++ ", " ++ onlyHours xs ++ ", " ++ onlyMins xs ++ " and " ++ onlySec xs -- years + hours + minutes + seconds
 | anyYears xs &&  not (anyDays xs) && not (anyHours xs) && anyMins xs && anySeconds xs = onlyYears xs ++ ", " ++ onlyMins xs ++ " and " ++ onlySec xs-- years + minutes + seconds
 | anyYears xs &&  anyDays xs && not (anyHours xs) && not(anyMins xs) && anySeconds xs = onlyYears xs ++ ", " ++ onlyDays xs ++ " and " ++ onlySec xs -- years + days + seconds
 | anyYears xs &&  anyDays xs && not (anyHours xs) && not(anyMins xs) && not(anySeconds xs) = onlyYears xs ++ " and " ++ onlyDays xs--years + days
 | anyYears xs &&  not (anyDays xs) && not (anyHours xs) && not(anyMins xs) && anySeconds xs = onlyYears xs ++ " and " ++ onlySec xs -- years + seconds
 | anyYears xs &&  not (anyDays xs) && anyHours xs&& not(anyMins xs) && not (anySeconds xs) = onlyYears xs ++ " and " ++ onlyHours xs -- years + hours
 | anyYears xs &&  anyDays xs && anyHours xs && not(anyMins xs) && not(anySeconds xs) = onlyYears xs ++ ", " ++ onlyDays xs ++ " and " ++ onlyHours xs--years + days + hours
 | anyYears xs &&  anyDays xs && not (anyHours xs) && anyMins xs && not(anySeconds xs) = onlyYears xs ++ ", " ++ onlyDays xs ++ " and " ++ onlyMins xs--years + days + minutes
 | anyYears xs &&  not (anyDays xs) && anyHours xs && not(anyMins xs) && anySeconds xs = onlyYears xs ++ ", " ++ onlyHours xs ++ " and " ++ onlySec xs-- years + hours + seconds
 | anyYears xs &&  not (anyDays xs) && anyHours xs && anyMins xs && not(anySeconds xs) = onlyYears xs ++ ", " ++ onlyHours xs ++ " and " ++ onlyMins xs-- years + hours + minutes
 | not (anyYears xs) && anyDays xs && not( anyHours xs) && anyMins xs && anySeconds xs = onlyDays xs ++ ", " ++ onlyMins xs ++ " and " ++ onlySec xs-- days + minutes + seconds 
 | not (anyYears xs) && anyDays xs && anyHours xs && not(anyMins xs) && anySeconds xs = onlyDays xs ++ ", " ++  onlyHours xs ++ " and " ++ onlySec xs--- days + hours + seconds
 | not (anyYears xs) && anyDays xs && anyHours xs && anyMins xs && not (anySeconds xs) = onlyDays xs ++ ", " ++  onlyHours xs ++ " and " ++ onlyMins xs--- days + hours + minutes
 | not (anyYears xs) && anyDays xs && not( anyHours xs) && anyMins xs && not(anySeconds xs) = onlyDays xs ++ " and " ++ onlyMins xs-- days + minutes 
 | not (anyYears xs) && anyDays xs && not( anyHours xs) &&  not (anyMins xs) && anySeconds xs = onlyDays xs ++ " and "  ++ onlySec xs-- days + seconds
 | not (anyYears xs) && anyDays xs && anyHours xs &&  not (anyMins xs) && not(anySeconds xs) = onlyDays xs ++ " and "  ++ onlyHours xs-- days + hours
 | anyYears xs &&  anyDays xs && anyHours xs && anyMins xs && not(anySeconds xs) = onlyYears xs ++ ", " ++ onlyDays xs ++ ", " ++ onlyHours xs ++ " and " ++ onlyMins xs -- years, days, hours and minutes
 | anyYears xs &&  anyDays xs && anyHours xs && not(anyMins xs) && anySeconds xs = onlyYears xs ++ ", " ++ onlyDays xs ++ ", " ++ onlyHours xs ++ " and " ++ onlySec xs -- years, days, hours and seconds
 | anyYears xs &&  anyDays xs && not(anyHours xs) && anyMins xs && anySeconds xs = onlyYears xs ++ ", " ++ onlyDays xs ++ ", " ++ onlyMins xs ++ " and " ++ onlySec xs -- years, days, minutes and seconds
 | anyYears xs &&  not(anyDays xs) && anyHours xs && anyMins xs && anySeconds xs = onlyYears xs ++ ", " ++ onlyHours xs ++ ", " ++ onlyMins xs ++ " and " ++ onlySec xs -- years, hours, minutes and seconds
 | otherwise = onlyYears xs ++ ", " ++ onlyDays xs ++ ", " ++ onlyHours xs ++ ", " ++ onlyMins xs ++ " and " ++ onlySec xs -- years + days + hours + minutes + seconds
 

formatDuration :: Int -> String
formatDuration x = getStringWithTime $ getListOfTimes x