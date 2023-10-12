-- File: StillAlive.hs
-- Author: TryFailTryAgain
-- Copyright (c) 2023. All rights reserved. For use in Open-Source projects this
-- may be freely copied or excerpted with credit to the author.
module StillAlive where

import System.Random (randomR, mkStdGen, randomRIO)
import System.IO
import Control.Concurrent (threadDelay)

introChat :: IO ()
introChat = do
    --talk to the user, make them feel welcome
    putStr "\ESC[2J" --clears the terminal
    putStr "\ESC[0m" --default

    --This is a little joke from the video game Portal 2 where Wheatly is trying to cover up that no one is around anymore
    pretendTypeVeryFast "01101001 01100110 00100000 01100001 01101110 \n01111001 01101111 01101110 01100101 00100000 \n\
        \01100001 01110011 01101011 01110011 00101100 \n00100000 01110100 01100101 01101100 01101100 \n00100000 01110100 \
        \01101000 01100101 01101101 \n00100000 01100001 01110011 00100000 01100110 \n01100001 01110010 00100000 01100001 \
        \01110011 \n00100000 01111001 01101111 01110101 00100000 \n01101011 01101110 01101111 01110111 00101100 \n00100000 \
        \01110100 01101000 01100101 00100000 \n01101100 01100001 01110011 01110100 00100000 \n01110100 01101001 01101101 \
        \01100101 00100000 \n01111001 01101111 01110101 00100000 01100011 \n01101000 01100101 01100011 01101011 01100101 \
        \\n01100100 00101100 00100000 01100101 01110110 \n01100101 01110010 01111001 01101111 01101110 \n01100101 00100000 \
        \01101100 01101111 01101111 \n01101011 01100101 01100100 00100000 01110000 \n01110010 01100101 01110100 01110100 \
        \01111001 \n00100000 01101101 01110101 01100011 01101000 \n00100000 01100001 01101100 01101001 01110110 \n01100101 \
        \00101110 00100000 01000001 01101100 \n01110010 01101001 01100111 01101000 01110100 \n00111111 00100000 01001110 \
        \01101111 01110100 \n00100000 01100100 01100101 01100001 01100100 \n00101110" --joke from Portal 2 as if we are catching Wheatly in conversation
    
    putStr "\n \n" --make some space
    putStr "\ESC[36m" --purple
    pretendType "Oh- hello there!"
    pretendThink "..."
    pretendType " My bad, I hadn't noticed you walk in.\n"
    pretendType "Im just your average program, doin what I do best\n"
    pretendType "Ya know,"
    pretendThink " "
    pretendType "crunchin numbers and conducting tests on some pe-"
    pretendThink " "
    pretendType " ROBOTS down here\n"
    pretendThink "any"
    pretendType "way, why dont we test those typing fingers of yours and see how fast you can type?\n"
    threadDelay 2500000
    putStr "\ESC[0m" --default
    pretendTypeFast "Ready to get started? Just hit one of those lettery keyes there infront of you... anyone will do:\n"


explainGame :: IO ()
explainGame = do
    putStr "\n \n" --make some space
    putStr "\ESC[36m" --cyan
    pretendType "After you confirm and the countdown ends, Ill give you a randomized sentence\n"
    pretendType "It will appear at the bottom of the screen with a white cursor below\n"
    pretendType "This is where you'll type out the sentence provided to you as best you can\n"
    pretendType "You can type it out as fast as you want, but of course the faster the better\n"
    pretendType "I'll keep track of how many words you get right and how long it takes you to type it out\n"
    pretendType "Once you're done, just hit the enter key and I'll do the rest\n\n"
    putStr "\ESC[0m" --default
    pretendThink "Rem"
    pretendType "ember:\n"
    pretendType "If you make a mistake"
    pretendThink ", "
    pretendType "just skip that word by pressing the space bar, or finish typing whats left over\n"
    putStr "\ESC[36m" --cyan
    threadDelay 2500000
    pretendTypeFast "Just like the first time"
    pretendThink ".."
    pretendType "hit one of those lettery keyes there infront of you... anyone should do the trick:\n"
    putStr "\ESC[0m" --default

gameCountdown :: IO ()
gameCountdown = do
    pretendType "\nOk now that's splendid, here we go!\n"
    pretendType "3"
    pretendThink "."
    pretendType "2"
    pretendThink "."
    pretendType "1"
    pretendThink "."
    putStr "\ESC[32m" --green
    pretendTypeFast "GO!\n"
    putStr "\ESC[0m" --default

--Joke quote from Portal 2 if the user is super slow
gentleInsult :: IO ()
gentleInsult = do --joke quote from Portal 2
    pretendType "slower than the average person. now "
    pretendThink "uh"
    pretendType ", \n\nMost test subjects do experience some, uh, \
        \cognitive deterioration after a few months in suspension. \nNow, you've been under for quite \
        \a lot longer, and it's *"
    pretendThink "not"
    pretendType "* out of the question that you might have a *very* minor case of \
        \serious brain damage.\nBut don't be alarmed, all right? Uh, although if you do feel alarmed"
    pretendThink ", "
    pretendType "\ntry to hold on to that feeling because that is the proper reaction\
        \to being told that you've got brain damage.\n\n"


--slowly print each character of a string as to simulate life
pretendType :: String -> IO ()
pretendType [] = return ()
pretendType (x:xs) = do
    putChar x
    delay <- randomRIO(10000, 100000)
    threadDelay delay
    pretendType xs 

--type it a a lot faster
pretendTypeFast :: String -> IO ()
pretendTypeFast [] = return ()
pretendTypeFast (x:xs) = do
    putChar x
    delay <- randomRIO(1000, 5000)
    threadDelay delay
    pretendTypeFast xs

pretendTypeVeryFast :: String -> IO ()
pretendTypeVeryFast [] = return ()
pretendTypeVeryFast (x:xs) = do
    putChar x
    delay <- randomRIO(100, 500)
    threadDelay delay
    pretendTypeVeryFast xs

--type it a lot slower
pretendThink :: String -> IO ()
pretendThink [] = return ()
pretendThink (x:xs) = do
    putChar x
    delay <- randomRIO(700000, 1000000)
    threadDelay delay
    pretendThink xs