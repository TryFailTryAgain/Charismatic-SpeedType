-- File: TextProcessing.hs
-- Author: TryFailTryAgain
-- Copyright (c) 2023. All rights reserved. For use in Open-Source projects this
-- may be freely copied or excerpted with credit to the author.
module TextProcessing where

import System.IO
import Data.IORef
import System.IO.Unsafe
import Markov --local Markov.hs for generation

--magic hack for a savable global variable-- 
--in hindsight I probably didn't need to do this, todo: rework
generatedSentence :: IORef String
generatedSentence = unsafePerformIO (newIORef "The default sentence is very dull so let's change that") --unsafePerformIO was the only solution I could 
                                                                                    --find to make this work. Just took looking at the error
                                                                                    -- messages and trying different things until it worked
getNewGameSentence = do
    newSentence <- generateSentence --calls generateSentence from MarkovGen
    writeIORef generatedSentence newSentence
    return newSentence


--kinda the entire game, whoops...--
--could totally refactored a lot more (๑-﹏-๑)
getInput :: Int -> Int -> IO String
getInput currentWordPos currentCharPos = do
    --some quick use data--
    gameSentence <- readIORef generatedSentence --due to originally testing with a static global variable before generation was added
                                       --this is to facilitate not needing to send the gameSentence with each call to getInput or others

    --out of bounds detection for words
    let maxWordPos = length (words gameSentence) - 1 --calculate the max number od words in the sentence
    let outOfWords = checkWordsOutOfBounds maxWordPos currentWordPos --set to true if out of bounds

    --out of bounds detection for chars
    let maxCharPos = length (words gameSentence !! currentWordPos) -1 --calculate the max number of chars in the current word
    let outOfChars = checkCharsOutOfBounds maxCharPos currentCharPos --set to true if out of bounds
    
    --start grabbing user input--
    c <- getChar
    case c of
        '\n' -> do --end of user input detected
            putChar '\n'
            return "\n"

        ' ' -> do --detect the spacebar and move to the next word and print the remaining chars in the current word in red if untyped
            --prevent skipping entire words with a double space
            if currentCharPos == 0 then do
                cs <- getInput currentWordPos currentCharPos --just enters another empty space into the string
                                                             --that will be filtered out later, but doesnt progress
                                                             -- the char position for checking
                return (c:cs)
            else do
                printRemainingChars currentWordPos currentCharPos maxWordPos maxCharPos --prints whatever is left of the current word in red
                putChar c --still need to print the space
                let nextWord = currentWordPos + 1 --iterate the word position as after a space we assume the next word is going to start
                let nextChar = 0 --resets car position for new word
                cs <- getInput nextWord nextChar
                return (c:cs) --recursively returns after all the chars are typed
        
        --detect backspace to allow for correction, but that seems to be impossible to detect using my method.
        --this mean the user is able to focus on the speed of words completed rather than error rate and just
        --moving along without losing pace. So in the end this lack of functionality is now a feature, not a bug

        _ -> do
            --quick logic check if we are out of words or chars as to not cause an out of bounds error for expectedWordStr and expectedChar
            if outOfWords || outOfChars then do
                printCharRed c --out of bounds thus print red as more chars are clearly wrong, so no check needed
                cs <- getInput currentWordPos currentCharPos
                return (c:cs) --recursively returns after all the chars are typed
            else do
                let expectedWordStr = (words gameSentence) !! currentWordPos --we extract the word we are expecting from the sentence as a substring
                let expectedChar = expectedWordStr !! currentCharPos --extracted the expected char from the expected word
                if c == expectedChar then --if the typed char is the same as the expected char
                    printCharGreen c --print green for correct
                else
                    printCharRed c --print red for incorrect
                cs <- getInput currentWordPos (currentCharPos + 1) --iterate the char position and loop
                return (c:cs) --recursively returns after all the chars are typed

--out of bounds checks for words
checkWordsOutOfBounds :: Int -> Int -> Bool
checkWordsOutOfBounds maxWordPos currentWordPos = if
    currentWordPos > maxWordPos then True 
        else False

--out of bounds checks for chars
checkCharsOutOfBounds :: Int -> Int -> Bool
checkCharsOutOfBounds maxCharPos currentCharPos = if
    currentCharPos > maxCharPos then True 
        else False

--print provided char in green
printCharGreen :: Char -> IO ()
printCharGreen c = do
    putStr "\ESC[32m"
    putChar c
    putStr "\ESC[0m"

--print provided char in red
printCharRed :: Char -> IO ()
printCharRed c = do
    putStr "\ESC[31m"
    putChar c
    putStr "\ESC[0m"

--bulk sentence checker from original implementation
checkWordsNum :: [String] -> [String] -> Int
checkWordsNum [] [] = 0
checkWordsNum (x:xs) (y:ys) = if x == y then 
    1 + checkWordsNum xs ys 
    else checkWordsNum xs ys
checkWordsNum _ _ = 0

--makes a list of the words that were typed correctly
correctlyTypedList :: [String] -> [String] -> [String]
correctlyTypedList [] [] = []
correctlyTypedList (x:xs) (y:ys) = if x == y then 
    x : correctlyTypedList xs ys 
    else correctlyTypedList xs ys
correctlyTypedList _ _ = []

printRemainingChars :: Int -> Int -> Int -> Int -> IO ()
printRemainingChars currentWordPos currentCharPos maxWordPos maxCharPos = do
    gameSentence <- readIORef generatedSentence --still need to pull the sentence from "global" to work without significant rewriting
    if currentWordPos > maxWordPos then
        return ()
    else if currentCharPos > maxCharPos then
            return ()
        else do
            let expectedWordStr = (words gameSentence) !! currentWordPos -- !! is the list index operator, it returns the value AT that position
            let expectedChar = expectedWordStr !! currentCharPos  -- !! is not NOT
            printCharRed expectedChar
            printRemainingChars currentWordPos (currentCharPos + 1) maxWordPos maxCharPos
            return ()

--cheap one off wpm calculator
--calculate the words per minute that the user typed correctly based on the correct words and the time it took to type them
calcWpm :: Float -> Int -> Float
calcWpm time numCorrect = if (numCorrect > 0) then do
        let timeInMinutes = time / 60.0
        ((fromIntegral numCorrect) / timeInMinutes)
    else 0
