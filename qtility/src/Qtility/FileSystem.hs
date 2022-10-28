module Qtility.FileSystem
  ( FileSystem,
    FileSystemWrite (..),
    FileSystemRead (..),
  )
where

import RIO
import RIO.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
  )

class (FileSystemRead m, FileSystemWrite m) => FileSystem m

class (Monad m) => FileSystemWrite m where
  writeFileM :: FilePath -> Text -> m ()
  writeByteStringFileM :: FilePath -> ByteString -> m ()
  makeDirectoryM :: Bool -> FilePath -> m ()
  removeFileM :: FilePath -> m ()
  removeDirectoryM :: FilePath -> m ()

instance FileSystemWrite IO where
  writeFileM = writeFileUtf8
  writeByteStringFileM = writeFileBinary
  makeDirectoryM = createDirectoryIfMissing
  removeFileM = removeFile
  removeDirectoryM = removeDirectoryRecursive

class (Monad m) => FileSystemRead m where
  readFileM :: FilePath -> m Text
  readByteStringFileM :: FilePath -> m ByteString
  listDirectoryM :: FilePath -> m [FilePath]
  doesFileExistM :: FilePath -> m Bool
  doesDirectoryExistM :: FilePath -> m Bool

instance FileSystemRead IO where
  readFileM = readFileUtf8
  readByteStringFileM = readFileBinary
  listDirectoryM = listDirectory
  doesFileExistM = doesFileExist
  doesDirectoryExistM = doesDirectoryExist
