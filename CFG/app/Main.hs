module Main where

import Lib
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStr ""
  putStrLn "======================================="
  putStrLn "======================================="
  putStrLn "       (Deutschland & Azerbaycan)"
  putStrLn "======================================="
  putStrLn "======================================="
  menuLoop

menuLoop :: IO ()
menuLoop = do
  putStrLn "\nМеню:\n 1 - Сгенерировать (Plusquamperfekt)🇩🇪\n       2 - Проверить своё (Plusquamperfekt)🇩🇪\n 3 - Сгенерировать (Präsens)🇩🇪\n        4 - Проверить своё (Präsens)🇩🇪\n 5 - Сгенерировать (Modal + Infinitiv)🇩🇪\n      6 - Проверить своё (Modal + Infinitiv)🇩🇪\n 7 - Сгенерировать (Азербайджанский: ADV+OBJ Present)🇦🇿\n         8 - Проверить своё (Азербайджанский: ADV+OBJ Present)🇦🇿\n 0 - Выход"
  putStr "> "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> deriveAndShow deriveRandomPluperfect >> menuLoop
    "2" -> askAndCheck "Plusquamperfekt" isPluperfect >> menuLoop
    "3" -> deriveAndShow deriveRandomPresent >> menuLoop
    "4" -> askAndCheck "Prasens" isPresent >> menuLoop
    "5" -> deriveAndShow deriveRandomModal >> menuLoop
    "6" -> askAndCheck "Modal + Infinitiv" isModal >> menuLoop
    "7" -> deriveAndShow deriveRandomAzeri >> menuLoop
    "8" -> askAndCheck "Азербайджанский" isAzeri >> menuLoop
    "0" -> putStrLn "Sagolun / Aufiderzein!"
    ""  -> putStrLn "Sagolun / Aufiderzein!"
    _   -> putStrLn "Выберите пункт меню!!!" >> menuLoop
  where
    deriveAndShow gen = gen >>= putStrLn . ("Сгенерированое: " ++)
    askAndCheck tag predF = do
      putStrLn $ "Введите предложение (" ++ tag ++ "):"
      putStr " >> "
      hFlush stdout
      sent <- getLine
      putStrLn $ if predF sent then "✅" else "❌"