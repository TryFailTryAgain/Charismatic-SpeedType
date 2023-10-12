--Requirements--
Markov.hs uses: import Data.MarkovChain (run, runMulti)
this requires the package markov-chain from hackage 
install with 'stack install markov-chain'
I attempted to get it to work with a stack.yaml file it refused to build.

--Usage--
'stack Main.hs'
or
'stack ghci', ':load Main', 'main'


--Notes for future reference--

--ANSI escape codes for coloring text and formatting
	https://en.wikipedia.org/wiki/ANSI_escape_code
	alternative I found too late
	https://hackage.haskell.org/package/ansi-terminal-0.6.2.3/docs/System-Console-ANSI.html
	
	putStr \ESC[2J --clears the terminal
	
--Recursive IO section provides a very basic look at what IORef does. Hoogle gave no indication
	for its use.
	https://www.michaelburge.us/2017/08/15/how-do-i-modify-a-variable-in-haskell.html#state-monad
	
--Markov Chain implementation package requres install with stack install markov-chain
	https://hackage.haskell.org/package/markov-chain
	https://en.wikipedia.org/wiki/Markov_chain#Examples
	
	
