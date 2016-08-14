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
      (String path) -> do
        let path' = unpack path
        result <- lookupSymbol loadPath
        case result of
          List car cdr -> do
            result' <- findM (flip lookupFile path') (car <| cdr)
            case result' of
              Nothing -> raise "load-error" $ "Cannot find file: " <> path
              Just file -> do
                parsed <- parseFile file
                mapM_ (compile >=> eval) parsed
                return Nil
          _ -> raiseTypeMismatch "list" result
      _ -> raiseTypeMismatch "string" val

lookupFile :: Value -> FilePath -> LispM (Maybe FilePath)
lookupFile (String dir) path = do
  let fullPath = joinPath [unpack dir, addExtension path ".lisp"]
  exists <- liftIO $ doesFileExist fullPath
  return $ if exists then Just fullPath else Nothing
lookupFile _ _ = return Nothing

findM :: (Monad m, Traversable f) => (a -> m (Maybe b)) -> f a -> m (Maybe b)
findM f =
  let go Nothing val = f val
      go result@(Just _) _ = return result
  in  foldlM go Nothing
