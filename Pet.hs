type Pet = (Int, Int, Int)
type Action = Pet -> Pet

maxStat = 10

newPet :: Pet
clamp :: Int -> Int
tick, feed, play, sleep, snack, feast :: Action
feedBy :: Int -> Action
bar :: Int -> String
statLabels :: [String]
statPanel :: Pet -> [String]

newPet = (5, 5, 5)
statLabels = ["Hunger", "Happiness", "Energy"]

clamp x
      | x < 0 = 0
      | x > maxStat = maxStat
      | otherwise = x

tick (h, p, e) = (h+1, p-1, e-1)

feed (h, p, e) = (clamp h+1, p, e)
play (h, p, e) = (h, clamp p+1, e)
sleep (h, p, e) = (h, p, clamp e+1)

feedBy x (h, p, e) = (clamp (h + x), p, e)
snack = feedBy 1
feast = feedBy 4

bar n = [if x <= n then '#' else '.' | x <- [1..maxStat]]
statPanel (h, p, e) = [label ++ ": " ++ bar v | (label, v) <- zip statLabels [h, p, e]]

