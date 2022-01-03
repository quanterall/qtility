module Qtility.Code where

import Control.Lens.Prism (_Just)
import Control.Monad.Catch (MonadMask)
import Language.Haskell.Interpreter
import Qtility.Code.Types
import RIO

execute ::
  forall a m.
  (Typeable a, MonadIO m, MonadMask m) =>
  [Import] ->
  String ->
  m (Either InterpreterError a)
execute importedModules text = do
  let imports =
        map
          ( \i ->
              ( i ^. importModule . unModuleName,
                i ^? importQualifiedName . _Just . unModuleName
              )
          )
          importedModules
  runInterpreter $ do
    setImportsQ imports
    interpret text (as :: a)
