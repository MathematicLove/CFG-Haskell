module Lib (
    deriveRandomPluperfect, deriveRandomPresent, deriveRandomModal, deriveRandomAzeri,
    isPluperfect, isPresent, isModal, isAzeri
  ) where

import System.Random (randomRIO)


type Terminal = String

randFrom :: [a] -> IO a
randFrom xs = (xs !!) <$> randomRIO (0, length xs - 1)

randMaybe :: [a] -> IO (Maybe a)
randMaybe xs = do
  flag <- randomRIO (0 :: Int, 1)
  if flag == 1 then Just <$> randFrom xs else pure Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing  = []

pronouns :: [Terminal]
pronouns = [ "ich", "du", "er", "sie", "es", "wir", "ihr", "Sie" ]

objects :: [Terminal]
objects =
  [ "den_Apfel", "das Buch", "die Katze", "den Hund",
    "mich", "dich", "ihn", "sie", "es", "nichts" ]

adverbs :: [Terminal]
adverbs = [ "heute", "gestern", "schon", "oft", "morgen" ]

auxsPlu :: [Terminal]
auxsPlu = [ "hatte", "hattest", "hatten", "hattet", "war", "warst", "waren", "wart" ]

pparts :: [Terminal]
pparts = [ "gesehen", "gegessen", "gemacht", "gegangen", "geschlafen", "geschrieben", "gesagt" ]

deriveRandomPluperfect :: IO String
deriveRandomPluperfect = do
  subj <- randFrom pronouns
  aux  <- randFrom auxsPlu
  advM <- randMaybe adverbs
  objM <- randMaybe objects
  ppar <- randFrom pparts
  pure $ unwords $ [subj, aux] ++ maybeToList advM ++ maybeToList objM ++ [ppar]

isPluperfect :: String -> Bool
isPluperfect str =
  case words str of
    [s1,s2,s3,s4]       -> s1 `elem` pronouns && s2 `elem` auxsPlu && s3 `elem` adverbs && s4 `elem` pparts
    [s1,s2,s3,s4]       -> s1 `elem` pronouns && s2 `elem` auxsPlu && s3 `elem` objects && s4 `elem` pparts
    [s1,s2,s3,s4,s5]    -> s1 `elem` pronouns && s2 `elem` auxsPlu && s3 `elem` adverbs && s4 `elem` objects && s5 `elem` pparts
    _                   -> False

verbsPresent :: [Terminal]
verbsPresent =
  [ "sehe", "siehst", "sieht", "sehen", "seht",
    "esse", "isst", "essen", "esst",
    "mache", "machst", "macht", "machen",
    "gehe", "gehst", "geht", "gehen",
    "schlafe", "schlAfst", "schlAft", "schlafen", "schlaft" ]

deriveRandomPresent :: IO String
deriveRandomPresent = do
  subj <- randFrom pronouns
  verb <- randFrom verbsPresent
  advM <- randMaybe adverbs
  obj  <- randFrom objects
  pure $ unwords $ [subj, verb] ++ maybeToList advM ++ [obj]

isPresent :: String -> Bool
isPresent str =
  case words str of
    [a,b,c]           -> a `elem` pronouns && b `elem` verbsPresent && c `elem` objects
    [a,b,c,d]         -> a `elem` pronouns && b `elem` verbsPresent && c `elem` adverbs && d `elem` objects
    _                 -> False

modals :: [Terminal]
modals = [ "will", "willst", "wollen", "wollt",
           "kann", "kannst", "kOnnen", "kOnnt",
           "muss", "musst", "mussen", "musst" ]

infinitives :: [Terminal]
infinitives = [ "sehen", "essen", "machen", "gehen", "schlafen", "schreiben", "sagen" ]

deriveRandomModal :: IO String
deriveRandomModal = do
  subj <- randFrom pronouns
  modal<- randFrom modals
  adv  <- randFrom adverbs
  objM <- randMaybe objects
  inf  <- randFrom infinitives
  pure $ unwords $ [subj, modal, adv] ++ maybeToList objM ++ [inf]

isModal :: String -> Bool
isModal str =
  case words str of
    [a,b,c,d]           -> a `elem` pronouns && b `elem` modals && c `elem` adverbs && d `elem` infinitives
    [a,b,c,d,e]         -> a `elem` pronouns && b `elem` modals && c `elem` adverbs && d `elem` objects && e `elem` infinitives
    _                   -> False

pronAze :: [Terminal]
pronAze = [ "men", "sen", "o", "biz", "siz", "onlar" ]

adverbsAze :: [Terminal]
adverbsAze = [ "bugun", "dunen", "tez-tez", "hemishe" ]

objectsAze :: [Terminal]
objectsAze = [ "kitabi", "evi", "alma", "mashini", "mektubu" ]

verbsAze :: [Terminal]
verbsAze = [ "oxuyur", "yazir", "gedir", "gorur", "alir", "sevir" ]

deriveRandomAzeri :: IO String
deriveRandomAzeri = do
  subj <- randFrom pronAze
  adv  <- randFrom adverbsAze
  obj  <- randFrom objectsAze
  verb <- randFrom verbsAze
  pure $ unwords [subj, adv, obj, verb]

isAzeri :: String -> Bool
isAzeri str =
  case words str of
    [a,b,c,d] -> a `elem` pronAze && b `elem` adverbsAze && c `elem` objectsAze && d `elem` verbsAze
    _         -> False
