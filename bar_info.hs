{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Shelly
import Data.Text as T hiding (map, tail)
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

numParser :: String -> Int
numParser input = case (parse numonly "failure" input) of
                Right str -> read str
                Left _ -> 0
    where
        numonly = do
            skipMany (noneOf "0123456789")
            many1 digit


str2doubles :: [String] -> [Double]
str2doubles = map read

-- Sums output from the command ps, used for cpu and mem
psOutSum :: Text -> Int
psOutSum = round . sum . str2doubles . map unpack . tail . map strip . T.lines

buildDeskText :: (Text, Text, Text, Text) -> Int -> Int -> Text
buildDeskText (act, inact, color1, color2) current total = "<" `append` before `append` cur `append` after `append` ">"
    where
        before = T.replicate (current - 1) (color2 `append` inact)
        cur = color1 `append` act
        after = T.replicate (total - current - 1) (color2 `append` inact)


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
        let cpu = psOutSum $ strip cpuList
        echo $ pack $ show cpu

        pmemList <- run "ps" ["-eo", "pmem"]
        let pmem = psOutSum $ strip pmemList
        echo $ pack $ show pmem

        curDesktop <- run "xprop" ["-root", "_NET_CURRENT_DESKTOP"]
        let currentDesktop = numParser $ unpack $ strip curDesktop
        echo $ pack $ show currentDesktop

        deskNum <- run "xprop" ["-root", "_NET_NUMBER_OF_DESKTOPS"]
        let desktopNumber = numParser $ unpack $ strip deskNum
        echo $ pack $ show desktopNumber

        let color2 = "%{F#ff42413f}"
        let color1 = "%{F#ff8a8987}"
        let deskText = buildDeskText ("↑", "↓", color1, color2) currentDesktop desktopNumber
        echo deskText


