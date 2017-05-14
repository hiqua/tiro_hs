-- My first real haskell program!
import Data.Char
import Data.Time
import Data.Typeable
import Text.Read
import Data.Maybe

split :: Char -> [Char] -> [String]
split _ [] = [[]]
split c (x:[]) = if c == x then [[]] else [[x]]
split c (x:xs) = if c == x then []:(split c xs)
			   else do
			      let sp = split c xs
			      (x:(head sp)): tail sp

concatenate :: Char -> [String] -> [Char]
concatenate _ [] = ""
concatenate _ [s] = s
concatenate c (x:y:xs) = x ++ [c] ++ (concatenate c (y:xs))

parse_int is = do
  let r = readMaybe is :: Maybe Int
  fromJust r

-- XXX: find explanation for '=>': monad?
format :: FormatTime t => t -> String
format t = formatTime defaultTimeLocale "%H:%M" t

-- compute_time_stamps :: TimeZone -> UTCTime -> [(Int , Int)] -> [String]
compute_time_stamps _ time [] = []
compute_time_stamps zone time ((h,m):xs) = do
  let length = realToFrac $ h * 60 * 60 + m * 60
  let ending_date = addUTCTime length time
  (format $ utcToZonedTime zone ending_date) : (compute_time_stamps zone ending_date xs)


main = do
  contents <- getContents
  ZonedTime _ zone <- getZonedTime
  let lines = split '\n' contents
  let decomposed_lines =  map (split ' ') $ filter (\s -> s /= "") lines
  curr <- getCurrentTime
  let hm = map (\line -> (parse_int $ head line, parse_int $ head $ tail line)) decomposed_lines
  let time_stamps = compute_time_stamps zone curr hm
  let activities = map (\line -> let _:_:act = line in act) decomposed_lines
  let act = map (concatenate ' ') activities
  let ta = zip time_stamps act

  putStrLn $ "-- " ++ format curr ++ " start"

  mapM (\line -> putStrLn $ "-> " ++ (fst line) ++ " " ++ (snd line)) ta
