module Main
    ( main
    )
where

import Control.Monad.Trans
import Distribution.ArchLinux.ALPM

import Data.Maybe

main :: IO ()
main = do
    r <- alpm hacmanMain
    case r of
        Left e ->
            print e

        Right () ->
            return ()

hacmanMain :: ALPM ()
hacmanMain = querySearchPackage ["haskell"]

query :: ALPM ()
query = do
    optionSetDatabasePath "/var/lib/pacman/"
    db <- optionGetLocalDatabase 
    packages <- databaseGetPackageCache db
    mapM_ displayPackageSimple packages 

queryGroup :: ALPM ()
queryGroup = do
    optionSetDatabasePath "/var/lib/pacman/"
    db <- optionGetLocalDatabase 
    groups <- databaseGetGroupCache db
    flip mapM_ groups $ \group -> do
        groupName <- groupGetName group
        packages <- groupGetPackages group
        flip mapM_ packages $ \package -> do
            packageName <- packageGetName package
            liftIO $ putStrLn $ groupName ++ ' ' : packageName
            
    return ()

querySearch :: ALPM ()
querySearch = do
    optionSetDatabasePath "/var/lib/pacman/"
    db <- optionGetLocalDatabase 
    packages <- databaseGetPackageCache db
    mapM_ displayPackage packages

querySearchPackage :: [String] -> ALPM ()
querySearchPackage packagenames = do
  optionSetDatabasePath "/var/lib/pacman/"
  db <- optionGetLocalDatabase
  packages <- databaseSearch db packagenames
  mapM_ displayPackage packages

displayPackageSimple :: Package -> ALPM ()
displayPackageSimple package = do
    name    <- packageGetName package
    version <- packageGetVersion package

    liftIO $ putStrLn $ name ++ ' ' : version

displayPackage :: Package -> ALPM ()
displayPackage package = do
    name        <- packageGetName package
    version     <- packageGetVersion package
    description <- packageGetDescription package
    groups      <- packageGetGroups package

    liftIO $ do
        putStr $ "local/" ++ name ++ ' ' : version
        if not $ null groups
            then do
                putStr " ("
                putStr $ foldl1 (\l r -> l ++ ' ' : r) groups
                putStrLn ")"
            else
                putStrLn ""
        putStrLn $ "    " ++ description
