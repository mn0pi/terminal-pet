module Tests where

import Pet
import Test.QuickCheck

test :: IO()

allPets, feedMisbehaves :: [Pet]
allEvents :: [Event]
allStatuses, unreachableStatuses :: [Status]
allMoods, unreachableMoods :: [Mood]

inRange :: Pet -> Bool

stepBreaksRange :: [(Event, Pet)]
feedByBreaksRange :: [(Int, Pet)]

toEvent :: Int -> Event

deadMismatch :: [Pet]

prop_splitReplay :: [Int] -> [Int] -> Bool
prop_clampRange, prop_clampIdempotent :: Int -> Bool

stats :: Pet -> (Int, Int, Int)

stats (Pet _ f h e) = (f, h, e)

allPets = [Pet "x" f h e | f <- [0..maxStat], h <- [0..maxStat], e <- [0..maxStat]]

allEvents = [Feed, Play, Sleep, Tick]

allStatuses = [Starving, Full, Elated, Depressed, Exhausted, Excited, Fine, Dead]
allMoods = [Happy, Sad, Energetic, Tired, Satisfied, Hungry, Neutral]

inRange (Pet _ f h e) = all ok [f, h, e]
                          where ok x = x >= 0 && x <= maxStat

stepBreaksRange = [(ev, p) | ev <- allEvents, p <- allPets, not (inRange (step ev p))]
feedByBreaksRange = [(n, p)| n <- [-12..12], p <- allPets, not (inRange (feedBy n p))]
unreachableStatuses = [s | s <- allStatuses, not (any (\p -> status p == s) allPets)]
unreachableMoods = [m | m <- allMoods, not (any (\p -> mood p == m) allPets)]

deadMismatch = [p | p <- allPets, (status p == Dead) /= allZero p]
                  where
                    allZero (Pet _ f h e) = f == 0 && h == 0 && e == 0

feedMisbehaves = [p | p <- allPets, not (ok p)]
                  where
                    ok p = let (f, h, e) = stats p
                               (f', h', e') = stats (feed p)
                           in f' >= f && h' == h && e' == e

toEvent n = allEvents !! (n `mod` length allEvents)

-- QUICKCHECK

prop_splitReplay xs ys = runEvents (evsA ++ evsB) p == runEvents evsB (runEvents evsA p)
                            where
                              evsA = map toEvent xs
                              evsB = map toEvent ys
                              p = newPet "x"

prop_clampRange n = clamp n >= 0 && clamp n <= maxStat
prop_clampIdempotent n = clamp (clamp n) == clamp n


-- Run Tests
test = do
          putStrLn ("cases where step leaves the range: " ++ show (take 5 stepBreaksRange))
          putStrLn ("cases where feedBy leaves the range: " ++ show (take 5 feedByBreaksRange))
          putStrLn ("cases where statuses are unreachable: " ++ show unreachableStatuses)
          putStrLn ("cases where moods are unreachable: " ++ show unreachableMoods)
          putStrLn ("cases where dead status is wrong: " ++ show deadMismatch)
          putStrLn ("cases where feed misbehaves: " ++ show feedMisbehaves)

          quickCheck prop_splitReplay
          quickCheck prop_clampRange
          quickCheck prop_clampIdempotent
