type Pet = (Int, Int, Int)

newPet :: Pet
newPet = (5, 5, 5)

clamp :: Int -> Int
clamp x
      | x < 0 = 0
      | x > 10 = 10
      | otherwise = x

tick :: Pet -> Pet
tick (x, y, z) = (x+1, y-1, z-1)
