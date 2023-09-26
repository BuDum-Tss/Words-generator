import Control.Monad
import System.Random
import Text.Regex.Posix
import Data.List
import Data.String

{-
list - list of words
k - number of words in the list
kw - number of generated words
lw - number of characters in a word
iw - number of intersection characters
s - generated piece of word
-}

--Generating a random number from 0 to k where the answer is the index of an element in a list of words
rand :: Int -> Int ->IO Int
rand f k= getStdRandom (randomR (f,k))

main :: IO ()
main = do
	--Entering user data
	putStrLn "===}---Word Generator---{==="
	putStrLn "Enter the number of words:"
	kWords<- getLine
	let kw = read kWords :: Int
	putStrLn "Enter the maximum word length:"
	lWords<- getLine
	let lw = read lWords :: Int
	putStrLn "Enter the minimum number of letters to intersect:"
	iWords<- getLine
	let iw = read iWords :: Int
	putStrLn "Enter the minimum number of letters to take:"
	tWords<- getLine
	let tw = read tWords :: Int
	putStrLn "Words:"
	--Loading data from the dictionary
	str <- readFile "Dictionary.txt"
	let list=getAllTextMatches $ str =~"[a-z]+"::[String]
	let k=(length list)-1
	--putStrLn (unlines list)
	--Printing a list of generated words
	c<-createWords list k kw lw iw tw
	putStrLn (unlines c)

--Generating a list of words
--kw is reduced by one, a new word is created, written to the list
createWords :: [String] -> Int -> Int -> Int -> Int -> Int ->IO [String]
createWords _ _ 0 _ _ _= do
	return []
createWords list k kw lw iw tw= do
	word <- newWord 0 list k lw iw tw ""
	c <- createWords list k (kw-1) lw iw tw
	if (goodWord list k word ==True)
		then return (word:c)
		else createWords list k kw lw iw tw
--Word verification
goodWord :: [String] -> Int -> String ->Bool
goodWord list k str = not(str `elem` list)
	&& ((str=~"[aeiouy]"::Int)>0) 
	&& ((str=~"[bcdfghijklmnpqrstvwxz]"::Int)>0) 
	&& ((str=~"[bcdfghijklmnpqrstvwxz]4"::Int)==0)
--Creating a new word
newWord ::  Int -> [String] -> Int -> Int -> Int -> Int -> String -> IO String
newWord try list k lw iw tw s = do 
	r<-rand 0 k
	t<-rand iw tw
	if (((length s)>= lw-tw) || (try>1000)) 
		then return s 
		else (newWord (try+1) list k lw iw tw (findWord (list !! r) iw t s ))

--Take a random word (the previous piece + part of the new one)
findWord :: String -> Int -> Int -> String -> String
findWord word iw t s = s++ splitt (drop ((length s)-iw) s) iw t word
--Highlights a piece of a long wt word
splitt :: String -> Int -> Int -> String -> String
splitt s iw t word = take t (drop (iw) (word =~(s++"[a-z]+")::String))