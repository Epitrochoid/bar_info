{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Shelly
import Data.Text as T
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token as P
import Data.Time
default (T.Text)

-- Parses the output of getZonedTime to "yyyy-mm-dd hh:mm:ss"
clockParser :: String -> Either ParseError String
clockParser = parse beforeDot "failure"
    where
        beforeDot = many (noneOf ".")

volParser :: String -> Either ParseError String
volParser = parse inBrackets "failure"
    where
        inBrackets = do
            skipMany (noneOf "[")
            anyToken
            manyTill anyChar (string "%")

avg :: [Double] -> Double
avg xs = (sum xs) / (fromIntegral $ Prelude.length xs) 

str2doubles :: [String] -> [Double]
str2doubles = Prelude.map read


main :: IO ()
main = shelly $ silently $ do
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

        volLvl <- run "amixer" ["get", "Master"]
        -- A bit of a kludge mixing parsec with prelude
        let volLevel = case (volParser $ Prelude.last $ Prelude.lines $ show volLvl) of
                           Right str -> str
                           Left error -> show error
        echo $ pack volLevel

        cpuList <- run "ps" ["-eo", "pcpu"]
        let cpu = sum $ str2doubles $ Prelude.map unpack $ Prelude.tail $ Prelude.map strip $ T.lines $ strip cpuList
        echo $ pack $ show cpu


