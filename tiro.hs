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
  (format $ utcToLocalTime zone ending_date) : (compute_time_stamps zone ending_date xs)


gen_lines contents curr zone = do
  let lines = filter (\s -> s /= "") $ split '\n' contents
  let decomposed_lines =  map (split ' ') lines

  let activities = map (\line -> let _:_:act = line in concatenate ' ' act) decomposed_lines
  let hm = map (\line -> (parse_int $ head line, parse_int $ head $ tail line)) decomposed_lines

  let time_stamps = compute_time_stamps zone curr hm

  let ta = zip time_stamps activities

  ("-- " ++ format (utcToLocalTime zone curr) ++ " start") : map (\line -> "-> " ++ fst line ++ " " ++ snd line) ta

round_up_time rd_unit utctime = do
  let UTCTime _ time = utctime
  let ceil_time = fromRational $ toRational $ secondsToDiffTime (ceiling (time/(60* rd_unit))) * rd_unit * 60

  addUTCTime ceil_time utctime

delay_time delay utctime = do
  let delay_nd = fromRational (delay * 60)
  addUTCTime delay_nd utctime

main = do
  currTime <- getCurrentTime

  let UTCTime day time = round_up_time 5 $ delay_time 110 $  currTime
  let curr = UTCTime day time

  ZonedTime _ zone <- getZonedTime
  contents <- getContents
  mapM (\line -> putStrLn line) $ gen_lines contents curr zone
