data Mood = Happy | Sad | Tired | Starving | Bored | Dead deriving (Show, Eq)
data Event = Feed | Play | Sleep | Tick deriving (Show, Eq)
data Pet = Pet String Int Int Int deriving (Show, Eq)

type Action = Pet -> Pet

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
isDead :: Pet -> Bool
runEvents :: [Event] -> Pet -> Pet
catchUp :: Int -> Pet -> Pet

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

mood (Pet name f h e)
      | f == 0 && h == 0 && e == 0 = Dead
      | e == 0 = Tired
      | h == 0 = Sad
      | f == 0 = Starving
      | otherwise = Happy

moodName p = show (mood p)

isDead p = mood p == Dead

runEvents [] p = p
runEvents (e:es) p = runEvents es (step e p)

catchUp 0 p = p
catchUp n p = catchUp (n-1) (tick p)


