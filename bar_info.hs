{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Shelly
import Data.Text as T
import Text.ParserCombinators.Parsec
import Data.Time
default (T.Text)

-- Parses the output of getZonedTime to "yyyy-mm-dd hh-mm-ss"
clockParser :: String -> Either ParseError String
clockParser = parse beforeDot "failure"
    where
        beforeDot = many (noneOf ".")


main :: IO ()
main = shelly $ verbosely $ do
        time <- liftIO getZonedTime
        let timestring = case (clockParser $ show time) of
                             Right str -> str
                             Left _ -> ""
        echo $ pack timestring

        batLvl <- readfile "/sys/class/power_supply/BAT0/capacity"
        let batLevel = strip batLvl
        echo batLevel

        batChrg <- readfile "/sys/class/power_supply/BAT0/status"
        let batCharging = case (unpack $ strip batChrg) of
                           "Unknown" -> True
                           "Discharging" -> False
                           _ -> False
        echo $ pack $ show batCharging

