{-# LANGUAGE DeriveDataTypeable #-} 
module Hacman.CommandLine 
  ( Command (..)
  , commandArgs 
  ) where

import System.Console.CmdArgs

data Command = Query
             { search       :: Bool
             , group        :: Bool
             , databasePath :: FilePath
             , parameters   :: [String]
             }
             | Sync 
             { search     :: Bool 
             , databasePath :: FilePath
             }
  deriving (Typeable,Data,Show)

query  = Query  { search = def
                , group  =  def
                , databasePath = def &= typDir &= name "dbpath" &= explicit 
                , parameters = def &= args 
                }
sync   = Sync   { search = def
                , databasePath = def &= typDir &= name "dbpath" &= explicit 
                }

commandArgs = cmdArgs (modes [query,sync])
