import Data.List
main :: IO()
main = do
    print $ getSunk database ==  [("Guadalcanal",["Kirishima"]),("North Atlantic",["Bismarck","Hood"]),("North Cape",["Schamhorst"]),("Surigao Strait",["Fuso","Yamashiro"])]
    print $ inBattleAfterDamaged database == ["California","Prince of Wales"]
 --   print $ getOutcomesForShip "California" outcomes
  --  print $ getOutcomesForShips outcomes outcomes
    -- print $ nub $ filteredNotOnlyDamaged (getOutcomesForShips outcomes outcomes)
    -- print $ getBattleForShip ("California",["ok","damaged"])  outcomes
    -- print $ getBattleForShip ("Prince of Wales",["damaged","ok"]) outcomes
    -- print $ getYearsofBattles ["Surigao Strait","Guadalcanal"] battles 
    -- print $ shipResultDates ("California",["ok","damaged"]) battles outcomes
    -- print $ afterDamaged ("California",["ok","damaged"],["1944-10-25","1942-11-15"])
    -- print $ allShipsResults ( nub $ filteredNotOnlyDamaged (getOutcomesForShips outcomes outcomes)) battles outcomes

type Name = String
type Date = String
type Class = String
type Result = String
type Launched = Int
data Battle = Battle Name Date
  deriving Show
data Ship = Ship Name Class Launched 
 deriving Show
data Outcome = Outcome Name Name Result 
 deriving Show
type Database = ([Outcome], [Battle], [Ship])

outcomes :: [Outcome]
outcomes = [ Outcome "Bismarck" "North Atlantic" "sunk", Outcome "California" "Surigao Strait" "ok", Outcome "Duke of York" "North Cape" "ok", Outcome "Fuso" "Surigao Strait" "sunk", Outcome "Hood" "North Atlantic" "sunk", Outcome "King George V" "North Atlantic" "ok", Outcome "Kirishima" "Guadalcanal" "sunk", Outcome "Prince of Wales" "North Atlantic" "damaged", Outcome "Rodney" "North Atlantic" "ok", Outcome "Schamhorst" "North Cape" "sunk", Outcome "South Dakota" "Guadalcanal" "damaged", Outcome "Tennessee" "Surigao Strait" "ok", Outcome "Washington" "Guadalcanal" "ok", Outcome "Prince of Wales" "Guadalcanal" "ok", Outcome "West Virginia" "Surigao Strait" "ok", Outcome "Yamashiro" "Surigao Strait" "sunk", Outcome "California" "Guadalcanal" "damaged" ]

battles :: [Battle]
battles = [ Battle "Guadalcanal" "1942-11-15", Battle "North Atlantic" "1941-05-25", Battle "North Cape" "1943-12-26",Battle "Surigao Strait" "1944-10-25" ]

ships :: [Ship]
ships = [ Ship "California" "Tennessee" 1921, Ship "Haruna" "Kongo"1916, Ship "Hiei" "Kongo" 1914, Ship "Iowa" "Iowa" 1943, Ship "Kirishima" "Kongo" 1915, Ship "Kongo" "Kongo" 1913, Ship "Missouri" "Iowa" 1944, Ship "Musashi" "Yamato"1942, Ship "New Jersey" "Iowa" 1943, Ship "North Carolina""North Carolina" 1941, Ship "Ramillies" "Revenge" 1917, Ship"Renown" "Renown" 1916, Ship "Repulse" "Renown" 1916, Ship"Resolution" "Renown" 1916, Ship "Revenge" "Revenge" 1916,Ship "Royal Oak" "Revenge" 1916, Ship "Royal Sovereign""Revenge" 1916, Ship "Tennessee" "Tennessee" 1920, Ship"Washington" "North Carolina" 1941, Ship "Wisconsin" "Iowa"1944, Ship "Yamato" "Yamato" 1941, Ship "Yamashiro" "Yamato"1947, Ship "South Dakota" "North Carolina" 1941, Ship"Bismarck" "North Carolina" 1911, Ship "Duke of York" "Renown"1916, Ship "Fuso" "Iowa" 1940, Ship "Hood" "Iowa" 1942, Ship"Rodney" "Yamato" 1915, Ship "Yanashiro" "Yamato" 1918, Ship "Schamhorst" "North Carolina" 1917, Ship "Prince of Wales""North Carolina" 1937, Ship "King George V" "Iowa" 1942, Ship"West Virginia" "Iowa" 1942 ]

database :: Database
database = (outcomes, battles, ships)

--hekper functions for first main function
filteredSunk :: [Outcome] -> [Outcome]
filteredSunk = filter (\ (Outcome _ _ result) -> result == "sunk")

getShipsInBattle :: String -> [Outcome]-> [Name]
getShipsInBattle _ [] = []
getShipsInBattle battle ((Outcome shipName battleName _):outcomes) = if battle == battleName then shipName : getShipsInBattle battle outcomes else getShipsInBattle battle outcomes

battlesWithSunkenShips :: [Outcome] -> [Outcome] -> [(Name, [Name])]
battlesWithSunkenShips [] _ = []
battlesWithSunkenShips ((Outcome shipName battleName _):outcomes) ogOutcomes = [(battleName, getShipsInBattle battleName ogOutcomes )] ++ battlesWithSunkenShips outcomes ogOutcomes


-- helper functions for second main function
filteredNotOnlyDamaged :: [(Name, [Result])] -> [(Name, [Result])]
filteredNotOnlyDamaged = filter (\ (_, results) -> elem "damaged" results && length results > 1)

getOutcomesForShip :: Name -> [Outcome] -> [Result]
getOutcomesForShip _ [] = []
getOutcomesForShip ship ((Outcome shipName battleName res) : outcomes) = if ship == shipName then res : getOutcomesForShip ship outcomes else getOutcomesForShip ship outcomes 

getOutcomesForShips :: [Outcome] -> [Outcome]  -> [(Name, [Result])]
getOutcomesForShips [] _ = []
getOutcomesForShips ((Outcome shipName battleName res) : outcomes) ogOutcomes = (shipName, getOutcomesForShip shipName ogOutcomes) : getOutcomesForShips outcomes ogOutcomes

getBattleForShip ::  (Name, [Result]) -> [Outcome] ->  [Name]
getBattleForShip  _ [] = []
getBattleForShip  s@(ship, res)  ((Outcome shipName battleName _):outcomes) = if ship == shipName then  battleName : getBattleForShip s outcomes else getBattleForShip s outcomes 

getYearOfBattle :: Name -> [Battle] -> Date
getYearOfBattle _ [] = []
getYearOfBattle battle ((Battle name date) : battlesS) = if battle == name then date else getYearOfBattle battle battlesS

getYearsofBattles :: [Name] -> [Battle] -> [Date]
getYearsofBattles [] _ = []
getYearsofBattles [name] battlesS = [getYearOfBattle name battlesS]
getYearsofBattles (name:names) battlesS = getYearOfBattle name battlesS : getYearsofBattles names battlesS

shipResultDates :: (Name, [Result]) -> [Battle] -> [Outcome] -> (Name, [Result],[Date])
shipResultDates s@(ship, results) battlesS outcomes = (ship, results, getYearsofBattles (getBattleForShip s outcomes) battlesS)

allShipsResults :: [(Name, [Result])] -> [Battle] -> [Outcome] -> [(Name, [Result],[Date])]
allShipsResults [] _ _ = []
allShipsResults (current:left) battles outcomes = shipResultDates current battles outcomes : allShipsResults left battles outcomes

getDateDamaged :: [Result]-> [Date]-> Date
getDateDamaged _ [] = error "not in database"
getDateDamaged [] _ = error "not in database"
getDateDamaged (res: results) (date:dates) = if res == "damaged" then date else getDateDamaged results dates

getDatesNotDamaged :: [Result]-> [Date]-> [Date]
getDatesNotDamaged _ [] = []
getDatesNotDamaged [] _ = []
getDatesNotDamaged (res: results) (date:dates) = if res /= "damaged" then date : getDatesNotDamaged results dates else getDatesNotDamaged results dates

getDateDamagedOfShip :: (Name, [Result],[Date]) -> Date
getDateDamagedOfShip s@(ship, results, dates) = getDateDamaged results dates

getDateNotDamagedOfShip :: (Name, [Result],[Date]) -> [Date]
getDateNotDamagedOfShip s@(ship, results, dates) = getDatesNotDamaged results dates

afterDamaged :: (Name, [Result],[Date]) -> Bool
afterDamaged s@(ship, results, dates) = dateDamaged < head datesNotDamaged
 where
   dateDamaged = getDateDamagedOfShip s
   datesNotDamaged =  sort $ getDateNotDamagedOfShip s

afterDamagedAll :: [(Name, [Result],[Date])]-> Bool
afterDamagedAll [] = True
afterDamagedAll (current:left) = afterDamaged current && afterDamagedAll left

getOnlyNames :: [(Name, [Result],[Date])]-> [Name]
getOnlyNames [] = []
getOnlyNames ((name, _, _ ) : left) = name : getOnlyNames left


--main functions
getSunk :: Database ->[(Name, [Name])]
getSunk ([],[],[]) = []
getSunk (outcomes, _, _) = sort $ nub $ battlesWithSunkenShips (filteredSunk outcomes) (filteredSunk outcomes) 

inBattleAfterDamaged :: Database -> [Name]
inBattleAfterDamaged (outcomes, battles, ships) = names
  where
    moreThanDamaged = nub $ filteredNotOnlyDamaged (getOutcomesForShips outcomes outcomes)
    results = allShipsResults moreThanDamaged battles outcomes
    areAllInBattle = afterDamagedAll results
    names = if areAllInBattle then getOnlyNames results else []
    
    