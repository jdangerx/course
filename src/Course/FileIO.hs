{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = getArgs >>= \args -> case args of
  (fp:.Nil) -> run fp
  _ -> putStrLn "We want one and only one argument, dingus."

-- get args, and stick the args into a fn that takes a list of args
-- and either runs or gives 'help'.


type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
-- run toc = do
  -- contents <- readFile toc
  -- getFiles (lines contents) >>= printFiles
run toc = readFile toc >>= getFiles . lines >>= printFiles

-- read the toc
-- `getFiles . lines` turns the toc into `List Chars` before `getFiles`ing them.
-- print out the gotten files

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles = sequence . map getFile

-- `map getFile` gets file for each fp in the list, sequence pulls the
-- whole list into a single IO

getFile ::
  FilePath
  -> IO (FilePath, Chars)
-- getFile fp = do
  -- contents <- readFile fp
  -- return (fp, contents)
getFile fp = readFile fp >>= \contents -> pure (fp, contents)

-- read the file
-- stick the filepath and contents into a pair and return

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles = void . sequence . (<$>) (uncurry printFile)

-- `map (uncurry printFile)` prints every file
-- `void . sequence` pulls them into an IO()

printFile ::
  FilePath
  -> Chars
  -> IO ()
-- printFile fp contents = do
--   putStrLn ("============ " ++ fp)
--   putStrLn contents
printFile fp contents = putStrLn ("============ " ++ fp) >>
                        putStrLn contents

-- f >> g ~= f >>= \_ -> g

