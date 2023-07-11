------------------------------------------------------------------------------
--- Library for dealing with URLs (Uniform Resource Locators).
---
--- @author Michael Hanus
--- @version July 2023
------------------------------------------------------------------------------

module System.URL ( getContentsOfURL, getContentsOfUrl ) where

import System.Directory ( getTemporaryDirectory )
import System.FilePath  ( (</>) )
import System.Path      ( getFileInPath )
import System.Process   ( getPID, system )
import System.IOExts    ( readCompleteFile )

--- Reads the contents of a document located by a URL.
--- This action requires that the program `curl` or `wget` is in your path,
--- otherwise an error is raised.
--- Included for backward compatibility.
getContentsOfUrl :: String -> IO String
getContentsOfUrl = getContentsOfURLwith False

--- Reads (quietly) the contents of a document located by a URL.
--- This action requires that the program "wget" is in your path,
--- otherwise the implementation must be adapted to the local
--- installation.
getContentsOfURL :: String -> IO String
getContentsOfURL = getContentsOfURLwith True

--- Reads (quietly) the contents of a document located by a URL.
--- This action requires that the program `curl` or `wget` is in your path,
--- otherwise an error is raised.
getContentsOfURLwith :: Bool -> String -> IO String
getContentsOfURLwith quiet url = do
  tmpdir <- getTemporaryDirectory
  pid    <- getPID
  let tmpfile = tmpdir </> "wgeturl." ++ show pid
  syscall <- getFetchCommand tmpfile
  system syscall
  cont <- readCompleteFile tmpfile
  system $ "rm -f "++tmpfile
  return cont
 where
  getFetchCommand tmpfile =
    getFileInPath "curl" >>=
    maybe
      (getFileInPath "wget" >>=
       maybe (error
                "Aborting `getContentsOfURL` since `curl` or `wget` not found")
             (\wget -> return $ wget ++ (if quiet then " -q " else " ") ++
                                "-O " ++ tmpfile ++ " \"" ++ url ++ "\""))
      (\curl -> return $ curl ++ (if quiet then " -s " else " ") ++
                         "-o " ++ tmpfile ++ " \"" ++ url ++ "\"")
