-- File: Markov.hs
-- Author: TryFailTryAgain
-- Copyright (c) 2023. All rights reserved. For use in Open-Source projects this
-- may be freely copied or excerpted with credit to the author.
module Markov where

import Data.MarkovChain (run, runMulti) --install with 'stack install markov-chain'
import System.Random (randomR, mkStdGen, randomRIO)
import Data.List (findIndices)

--open a file called "generationText.txt" and generate a sentence using the markov-chain package
generateSentence :: IO String
generateSentence = do
    randomInt <- randomRIO (1, 2600) --random number between 1 and 2600 as there are roughly 2600 characters in the file from the wonderful wizard of oz
    workingFile <- readFile "generationText.txt"
    let randomIntFixed = charsTillRandomWord workingFile randomInt --find the number of chars till the beginning of a random word
    let sentence = take 100 $ run 5 workingFile (randomIntFixed+1) (mkStdGen 1234) --generate a sentence of 100 chars
                                                                                   --run 5 is the shift value
                                                                                   --randomIntFixed+1 is the starting index, the +1 is to fix an off by one error I made
                                                                                   --mkStdGen 1234 is the seed value
    let sentenceFixed = findlastWord sentence
    return sentenceFixed

--test function to print the files contents as i had a few errors with IO String vs String type contradictions
printFile = do
    filetest <- readFile "generationText.txt"
    putStrLn filetest

--accept a string and a number and return the number of chars till the beginning of the 1st word after that number
--stops the beginning of words being cut off from the string starting index in the generate function
charsTillRandomWord :: String -> Int -> Int
charsTillRandomWord [] _ = 0
charsTillRandomWord x n = do
    if n == 0 then 0
    else do
        let currentChar = x !! n
        if currentChar == ' ' then n
        else charsTillRandomWord x (n+1)

--takes the generated sentence and returns a fixed string without the last element potentially being cut off
findlastWord :: String -> String
findlastWord [] = []
findlastWord x = do
    let allWhiteSpaceIndex = findIndices (== ' ') x --makes an array of indexes where all the white space is
    take (last allWhiteSpaceIndex) x --cuts the string at the last index of where a white space 
                                     --is thus removing the last word which is likely to be cut off
