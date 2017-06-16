-- My first real haskell program!
-- TODO
-- pauses between activities
-- global parameters as command line options
-- rewrite by using parsec
--   specifying the grammar of the input files
--   extract a parser directly from the grammar
import Data.Char
import Data.Time
import Data.Typeable
import Text.Read
import Data.Maybe

-- split the list of char at every occurrence of a char
split :: Char -> [Char] -> [String]
split _ [] = [[]]
split c (x:[]) = if c == x then [[]] else [[x]]
split c (x:xs) = if c == x then []:(split c xs) else do
      let hd:tl = split c xs
      (x:(hd)): tl

interlace _ [] = []
interlace _ [x] = [x]
interlace e (x : y : xs) = x : e : (interlace e (y:xs))

mergeStrings :: [String] -> String
mergeStrings [] = ""
mergeStrings (x : xs) = x ++ (mergeStrings xs)

-- concatenate multiple strings with a separation string
-- concatenate :: String -> [String] -> [Char]
concatenate :: String -> [String] -> String
concatenate c xs = mergeStrings $ interlace c xs

parseInt is = do
  let r = readMaybe is :: Maybe Int
  fromJust r

-- XXX: find explanation for '=>': monad?
format :: FormatTime t => t -> String
format t = formatTime defaultTimeLocale "%H:%M" t


computeTimeStamps :: TimeZone -> UTCTime -> [(Int , Int)] -> [String]
computeTimeStamps _ time [] = []
computeTimeStamps zone time ((h,m):xs) = do
   let length = realToFrac $ h * 60 * 60 + m * 60
   let ending_date = addUTCTime length time
   (format $ utcToLocalTime zone ending_date) : (computeTimeStamps zone ending_date xs)


genLines contents curr zone = do

  let decomposedLines =  map (split ' ') $ filter (\s -> s /= "") $ split '\n' contents

  let activities = map (\line -> let _:_:a = line in concatenate " " a) decomposedLines

  -- (h,m) list
  let hm = map (\l -> (parseInt $ head l, parseInt $ head $ tail l)) decomposedLines

  let timeStamps = computeTimeStamps zone curr hm

  let ta = zip timeStamps activities

  let header = ("-- " ++ format (utcToLocalTime zone curr) ++ " start")
  let tl = map (\line -> "-> " ++ fst line ++ " " ++ snd line) ta

  header : tl

roundUpTime :: DiffTime -> UTCTime -> UTCTime
roundUpTime rd_unit utctime = do
  -- hackish
  let rd_unit_int = fromEnum rd_unit
  let UTCTime _ time = utctime
  let truncatedTime = ceiling (time / (60 * rd_unit)) * 60 * rd_unit_int
  let deltaDt = (toEnum truncatedTime) - time
  let ndf = fromRational $ toRational $ deltaDt

  addUTCTime ndf utctime


delayTime delay utctime = do
  let delay_nd = fromRational (delay * 60)
  addUTCTime delay_nd utctime

getZone () = do
  ZonedTime _ zone <- getZonedTime
  return zone


-- addBreaks :: String -> Int -> String
-- addBreaks content duration = do
--   let lines = split '\n' content
--   let e = "0 " ++ show duration ++ " break"
--   concatenate "\n" (interlace e lines)

main = do
  currTime <- getCurrentTime
  zone <- getZone ()
  contents <- getContents
  -- let contents = addBreaks ontents 30
  putStrLn contents

  let modifiedCurrTime = roundUpTime 10 $ delayTime 5 $ currTime

  mapM (\line -> putStrLn line) $ genLines contents modifiedCurrTime zone
