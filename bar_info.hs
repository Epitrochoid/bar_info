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
        before = T.replicate current (color2 `append` inact)
        cur = color1 `append` act
        after = T.replicate (total - current - 1) (color2 `append` inact)


main :: IO ()
main = shelly $ silently $ do
        time <- liftIO getZonedTime
        let timestring = pack $ case (clockParser $ show time) of
                             Right str -> str
                             Left _ -> ""

        batLvl <- readfile "/sys/class/power_supply/BAT0/capacity"
        let batLevel = strip batLvl

        batChrg <- readfile "/sys/class/power_supply/BAT0/status"
        let batCharging = pack $ case (unpack $ strip batChrg) of
                           "Unknown" -> "↑"
                           "Discharging" -> "↓"
                           _ -> ""

        volLvl <- run "amixer" ["get", "Master"]
        -- A bit of a kludge mixing parsec with prelude
        let volLevel = pack $ case (volParser $ Prelude.last $ Prelude.lines $ show volLvl) of
                           Right str -> str
                           Left error -> show error

        cpuList <- run "ps" ["-eo", "pcpu"]
        let cpu = pack $ show $ psOutSum $ strip cpuList

        pmemList <- run "ps" ["-eo", "pmem"]
        let pmem = pack $ show $ psOutSum $ strip pmemList

        curDesktop <- run "xprop" ["-root", "_NET_CURRENT_DESKTOP"]
        let currentDesktop = numParser $ unpack $ strip curDesktop

        deskNum <- run "xprop" ["-root", "_NET_NUMBER_OF_DESKTOPS"]
        let desktopNumber = numParser $ unpack $ strip deskNum

        let color2 = "%{F#ff42413f}"
        let color1 = "%{F#ff8a8987}"
        let deskText = buildDeskText ("↑", "↓", color1, color2) currentDesktop desktopNumber

        let output = [color1, deskText, pack "%{c}", color2, timestring, pack "%{r}"
                     , color1, pack "CPU: ", color2, cpu, pack "%% "
                     , color1, pack "PMEM: ", color2, pmem, pack "%% "
                     , color1, pack "VOL: ", color2, volLevel, pack "%% "
                     , color1, pack "BAT: ", color2, batCharging, batLevel, pack "%%"]
        echo $ T.concat output




