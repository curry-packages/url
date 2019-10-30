------------------------------------------------------------------------------
--- Library for dealing with URLs (Uniform Resource Locators).
---
--- @author Michael Hanus
--- @version October 2019
------------------------------------------------------------------------------

module System.URL ( getContentsOfUrl ) where

import Directory ( getTemporaryDirectory )
import FilePath  ( (</>) )
import System    ( getPID, system )
import IOExts    ( readCompleteFile )

--- Reads the contents of a document located by a URL.
--- This action requires that the program "wget" is in your path,
--- otherwise the implementation must be adapted to the local
--- installation.
getContentsOfUrl :: String -> IO String
getContentsOfUrl url = do
  tmpdir <- getTemporaryDirectory
  pid    <- getPID
  let tmpfile = tmpdir </> "wgeturl." ++ show pid
  system $ "wget -O " ++ tmpfile ++ " \"" ++ url ++ "\""
  cont <- readCompleteFile tmpfile
  system $ "rm -f "++tmpfile
  return cont
