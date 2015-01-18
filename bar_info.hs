{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Shelly
import Data.Text as T
import Text.ParserCombinators.Parsec
import Data.Time
default (T.Text)

clockParser :: String -> Either ParseError String
clockParser = parse beforeDot "failure"
    where
        beforeDot = many (noneOf ".")


main :: IO ()
main = do
        time <- getZonedTime
        let timestring = case (clockParser $ show time) of
                             Right str -> str
                             Left _ -> ""
        putStrLn timestring
