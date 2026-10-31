module Pet where

data Mood = Happy | Sad | Energetic | Tired | Satisfied | Hungry | Neutral  deriving (Show, Eq)
data Status = Starving | Full | Elated | Depressed | Exhausted | Excited | Fine | Dead deriving (Show, Eq)
data Event = Feed | Play | Sleep | Tick deriving (Show, Eq)
data Pet = Pet String Int Int Int deriving (Show, Eq)

type Action = Pet -> Pet
type Memory = [Event]

newPet :: String -> Pet
clamp :: Int -> Int
tick, feed, play, sleep, snack, feast :: Action
feedBy :: Int -> Action
bar :: Int -> String
statLabels :: [String]
statPanel :: Pet -> [String]

step :: Event -> Pet -> Pet

mood :: Pet -> Mood
moodName :: Pet -> String

status :: Pet -> Status
statusName :: Pet -> String

isDead :: Pet -> Bool
runEvents :: [Event] -> Pet -> Pet
catchUp :: Int -> Pet -> Pet
percent :: Int -> Int -> Maybe Int
pct :: Int -> Int
tryStep :: Event -> Pet -> Maybe Pet
describeTry :: Event -> Pet -> String

remember :: Event -> Memory -> Memory
summary :: [Event] -> String

maxStat :: Int

maxStat = 10

newPet n = Pet n 5 5 5
statLabels = ["Fullness  ", "Happiness ", "Energy    "]

clamp x
      | x < 0 = 0
      | x > maxStat = maxStat
      | otherwise = x

tick (Pet name f h e) = Pet name (clamp (f-1)) (clamp (h-1)) (clamp (e-1))

feed (Pet name f h e) = Pet name (clamp (f+1)) h e
play (Pet name f h e) = Pet name f (clamp (h+1)) e
sleep (Pet name f h e) = Pet name f h (clamp (e+1))

step Feed p = feed p
step Play p = play p
step Sleep p = sleep p
step Tick p = tick p

feedBy x (Pet name f h e) = Pet name (clamp (f + x)) h e
snack = feedBy 1
feast = feedBy 4

bar n = [if x <= n then '#' else '.' | x <- [1..maxStat]]
statPanel (Pet name f h e) = [label ++ ": " ++ bar v | (label, v) <- zip statLabels [f, h, e]]

mood (Pet _ f h e)
      | f == 0 && h == 0 && e == 0 = Neutral
      | pct h > 66 = Happy
      | pct h < 34 = Sad
      | pct e > 66 = Energetic
      | pct e < 34 = Tired
      | pct f > 66 = Satisfied
      | pct f < 34 = Hungry
      | otherwise = Neutral

moodName p = show (mood p)

status (Pet _ f h e)
      | f == 0 && h == 0 && e == 0 = Dead
      | f == 0 = Starving
      | h == 0 = Depressed
      | e == 0 = Exhausted
      | h == maxStat = Elated
      | e == maxStat = Excited
      | f == maxStat = Full
      | otherwise = Fine

statusName p = show (status p)

isDead p = status p == Dead

runEvents ev p = foldl (flip step) p ev

catchUp 0 p = p
catchUp n p = catchUp (n-1) (tick p)

percent x 0 = Nothing
percent x y = Just (x `div` y)

pct x = x * 100 `div` maxStat

showPercent Nothing = "Error!"
showPercent (Just x) = show x

tryStep _ (Pet _ 0 0 0) = Nothing
tryStep Play (Pet _ _ _ 0) = Nothing
tryStep Feed (Pet _ 10 _ _) = Nothing
tryStep ev p = Just (step ev p)

describeTry _ (Pet _ 0 0 0) = "Pet is dead."
describeTry Play (Pet _ _ _ 0) = "Pet is too exhausted!"
describeTry Feed (Pet _ 10 _ _) = "Pet is full!"
describeTry ev p = show ev ++ " successful :)"

remember = (:)
summary ev = foldr (\e rest -> if null rest then show e else show e ++ ", " ++ rest) "" ev  


