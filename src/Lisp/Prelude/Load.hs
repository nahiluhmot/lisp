{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Lisp.Prelude.Load (defPreludeLoad) where

import Control.Monad.Except
import Data.Foldable
import Data.Monoid
import Data.Sequence
import Data.Text (pack, unpack)
import System.Directory
import System.FilePath.Posix
import Paths_lisp

import Lisp.Data
import Lisp.Core
import Lisp.VirtualMachine
import Lisp.Compiler
import Lisp.Parser

defPreludeLoad :: LispM ()
defPreludeLoad = do
  lib <- liftIO $ getDataFileName "lib/"
  cwd <- liftIO $ getCurrentDirectory
  loadPath <- symToID "*load-path*"
  def loadPath $ list . fmap (String . pack) $ [lib, cwd]

  defun1 "load" $ \val -> do
    case val of
      (String str) -> do
        let path' = unpack str
        result <- lookupSymbol loadPath
        case result of
          List car cdr -> do
            result' <- findM (flip lookupFile path') (car <| cdr)
            case result' of
              Nothing -> raise "load-error" $ "Cannot find file: " <> str
              Just file -> do
                parsed <- parseFile file
                mapM_ (compile >=> eval) parsed
                return Nil
          _ -> raiseTypeMismatch "list" result
      _ -> raiseTypeMismatch "string" val

lookupFile :: Value -> FilePath -> LispM (Maybe FilePath)
lookupFile (String dir) path =
  let dir' = unpack dir
      paths :: [FilePath]
      paths = [ joinPath [dir', addExtension path ".lisp"]
              , joinPath [dir', path]
              ]
      go Nothing file = do
        exists <- liftIO $ doesFileExist file
        return $ if exists then Just file else Nothing
      go v@(Just _) _ = return v
  in  foldlM go Nothing paths
lookupFile _ _ = return Nothing

findM :: (Monad m, Traversable f) => (a -> m (Maybe b)) -> f a -> m (Maybe b)
findM f =
  let go Nothing val = f val
      go result@(Just _) _ = return result
  in  foldlM go Nothing
