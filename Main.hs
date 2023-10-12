-- File: Main.hs
-- Author: TryFailTryAgain
-- Copyright (c) 2023. All rights reserved. For use in Open-Source projects this
-- may be freely copied or excerpted with credit to the author.
module Main where

import System.IO (hSetEcho, stdin, hSetBuffering, BufferMode(NoBuffering))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Markov (generateSentence) --local Markov.hs functions
import TextProcessing --local TextProcessing.hs functions
import StillAlive --local StillAlive.hs functions

main = do
    --setup stuff--
    hSetEcho stdin False --hides the user input as they type so we can reprint it in the correct colors
    hSetBuffering stdin NoBuffering --above^ + allows capturing of each keypress as it happens
    gameSentence <- getNewGameSentence --generate a new sentence on instance load 
    let talkingSKIP = False --set to 'True' to skip the chatting bits
    --gameSentence <- readIORef generatedSentence --unflag to use default sentence defined in generatedSentence, flag above

    --talk to the user--
    if talkingSKIP == False then do
        introChat
        waitForInput <- getChar
        explainGame
        waitForInput <- getChar
        gameLoop gameSentence
        return ()
    else do
        gameLoop gameSentence
        return ()


gameLoop :: String -> IO ()
gameLoop gameSentence = do
    --start the game loop asking if the user is ready to start typing
    putStr "\ESC[36m" --cyan
    pretendType "Ready? Enter Y / N : "
    answer <- getChar
    putStr "\ESC[0m" --default color, looks better for user inputs
    putChar answer
    putStr "\ESC[36m" --cyan

    if answer == 'N' || answer == 'n' then do --Check if user wants to exit, then let them go         
        pretendType "\nAlright, "
        pretendThink "s"
        pretendType "o a silent competition it is then.\n"
        return ()
    else if answer /= 'Y' && answer /= 'y' then do --Checks for invalid input, askes again, loops
        pretendTypeFast "\nI'm sorry, I didn't catch that.\n"
        gameLoop gameSentence
    else do --user entered 'Y' or 'y' so start the game
        gameCountdown --give a little countdown for fun
        putStrLn gameSentence --display the sentence to the user
        startTime <- getCurrentTime --start the timer after print just incase there is loading time for fairnes
        typed <- getInput 0 0 --grab the user input, start at the first word and first character. This loops
        endTime <- getCurrentTime --stop the timer

        --calculate the users stats--
        let typedWords = words typed --takes only the users direct input and breaks into words/array of words
        let typedCorrectly = correctlyTypedList (words gameSentence) typedWords --pulls out only correct words that the user typed into array of words
        let numCorrect = (length typedCorrectly) --returns the total number of correct words the user typed
        let time = realToFrac (diffUTCTime endTime startTime) --total time user took from start to finish
        let wpm = calcWpm time numCorrect --float in words per minute
        let wpmPercent = floor ((38-wpm)/((38+wpm)/2)*100)
        let wikiTime = floor ((((4000000000 / wpm) / 60) / 24) /365) --time to type all of wikipedia in years

        putStr "\ESC[36m" --cyan
        putStrLn ("You typed: " ++ "\ESC[32m" ++ show typedWords ++ "\ESC[36m")
        putStrLn ("You had " ++ "\ESC[32m" ++ show numCorrect ++ "\ESC[36m" ++ " correct words")
        putStrLn ("It took you " ++ "\ESC[32m" ++ show time ++ "\ESC[36m" ++ " seconds to finish")
        putStrLn ("Your WPM is: " ++ "\ESC[32m" ++ show wpm ++ "\ESC[36m")
        putStr ("The average persons WPM ~38, so you are " ++ "\ESC[32m" ++ show wpmPercent ++"\ESC[36m" ++ "% ")
        if wpm > 38 then
            putStrLn "faster than the average person!"
        else if wpm < 5 then gentleInsult --for a super slow typist, give them another Portal 2 Wheatly quote
            else putStrLn ("slower than the average person. Keep practicing!")
        putStrLn ("If you where to type all of Wikipedia it would take you " ++ "\ESC[32m" ++ show wikiTime ++ "\ESC[36m"++ " years!\n\n")
        putStrLn "Would you like to play again? Enter Y / N"
        putStr "\ESC[0m" --default
        if answer == 'N' || answer == 'n' then do
            putStr "\ESC[36m" --cyan
            putStrLn "\nOk, bye!"
            putStr "\ESC[0m" --default
            return () 
        else do
            gameSentence <- getNewGameSentence
            gameLoop gameSentence

