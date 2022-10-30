module Qtility.FileSystem
  ( FileSystem,
    WriteFileSystem (..),
    ReadFileSystem (..),
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

class (ReadFileSystem m, WriteFileSystem m) => FileSystem m

class (Monad m) => WriteFileSystem m where
  writeFileM :: FilePath -> Text -> m ()
  writeByteStringFileM :: FilePath -> ByteString -> m ()
  makeDirectoryM :: Bool -> FilePath -> m ()
  removeFileM :: FilePath -> m ()
  removeDirectoryM :: FilePath -> m ()

instance WriteFileSystem IO where
  writeFileM = writeFileUtf8
  writeByteStringFileM = writeFileBinary
  makeDirectoryM = createDirectoryIfMissing
  removeFileM = removeFile
  removeDirectoryM = removeDirectoryRecursive

class (Monad m) => ReadFileSystem m where
  readFileM :: FilePath -> m Text
  readByteStringFileM :: FilePath -> m ByteString
  listDirectoryM :: FilePath -> m [FilePath]
  doesFileExistM :: FilePath -> m Bool
  doesDirectoryExistM :: FilePath -> m Bool

instance ReadFileSystem IO where
  readFileM = readFileUtf8
  readByteStringFileM = readFileBinary
  listDirectoryM = listDirectory
  doesFileExistM = doesFileExist
  doesDirectoryExistM = doesDirectoryExist
