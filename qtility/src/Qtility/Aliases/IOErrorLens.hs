module Qtility.Aliases.IOErrorLens where

import Foreign.C.Types (CInt)
import RIO
import System.IO.Error (IOErrorType)
import qualified System.IO.Error.Lens as IOError

handleL :: Lens' IOException (Maybe Handle)
handleL = IOError.handle

locationL :: Lens' IOException String
locationL = IOError.location

fileNameL :: Lens' IOException (Maybe FilePath)
fileNameL = IOError.fileName

descriptionL :: Lens' IOException String
descriptionL = IOError.description

errnoL :: Lens' IOException (Maybe CInt)
errnoL = IOError.errno

errorTypeL :: Lens' IOException IOErrorType
errorTypeL = IOError.errorType
