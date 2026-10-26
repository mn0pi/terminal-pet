module Main where

import System.IO (hSetEncoding, stdout, utf8)

-- Run with:  runghc PetPreview.hs   (in kitty, so the colours show)

data Mood = Happy | Content | Grumpy | Starving | Dead deriving (Show, Eq)

-- The sprite is data: one letter per "pixel", 12 wide x 9 tall.
--   k outline   l highlight   b body   d shade
--   w belly     g belly edge  p blush  y beak   f feet   E eye
sprite :: [String]
sprite =
  [ "....kkkk...."
  , "..klllbbbk.."
  , ".klllbbbbdk."
  , ".klbEbbEbdk."
  , ".klpbyybpdk."
  , "kklbwwwwbdkk"
  , "kbkwwwwwwkdk"
  , ".kkgwwwwgkk."
  , "..ff....ff.."
  ]

-- 256-colour foreground/background plus a two-character glyph (one pixel = 2 columns)
paint :: Int -> Int -> String -> String
paint fg bg s = "\ESC[38;5;" ++ show fg ++ ";48;5;" ++ show bg ++ "m" ++ s ++ "\ESC[0m"

eye :: Mood -> String
eye Happy    = paint 17 98 "^^"
eye Content  = paint 17 98 "\9608\9608"   -- solid dark block
eye Grumpy   = paint 17 98 "\9600\9600"   -- flat lid
eye Starving = paint 17 98 "@@"
eye Dead     = paint 17 98 "xx"

cell :: Mood -> Char -> String
cell _ 'k' = paint 17  17  "  "
cell _ 'l' = paint 141 141 "  "           -- light purple highlight
cell _ 'b' = paint 98  98  "  "           -- purple body
cell _ 'd' = paint 55  55  "  "           -- deep purple shade
cell _ 'w' = paint 255 255 "  "
cell _ 'g' = paint 250 255 "\9618\9618"   -- medium shade over white: soft belly edge
cell _ 'p' = paint 217 98  "\9618\9618"   -- pink shade over purple: blush
cell _ 'y' = paint 216 216 "  "
cell _ 'f' = paint 209 209 "  "
cell m 'E' = eye m
cell _ _   = "  "

render :: Mood -> [String]
render m = map (concatMap (cell m)) sprite

-- The "same size" property from the spec
prop_sameWidth :: Bool
prop_sameWidth = all ((== 12) . length) sprite

main :: IO ()
main = do
  hSetEncoding stdout utf8
  putStrLn ("sprite rows all 12 wide: " ++ show prop_sameWidth)
  mapM_ (\m -> do
          putStrLn ("\n" ++ show m)
          mapM_ putStrLn (render m))
        [Happy, Content, Grumpy, Starving, Dead]
