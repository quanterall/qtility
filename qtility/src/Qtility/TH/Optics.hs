{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Qtility.TH.Optics where

import Control.Exception.Lens (exception)
import Control.Lens.TH (makeClassyPrisms)
import Language.Haskell.TH
import RIO

-- | Generates prisms for an error type, an instance of 'Exception' for them as well as a
-- boilerplate instance of the generated @As@ class for 'SomeException'. Note that this is not
-- useful for cases where your prism has two leading underscores, i.e. if you're using a @newtype@
-- as an exception type. With some more work we could solve this by looking at the type and asking
-- whether or not `makeClassyPrisms` would have generated a prism with two leading underscores, but
-- without doing more work it will not be solved.
makeClassyException :: Name -> DecsQ
makeClassyException name = do
  classyPrismOutput <- makeClassyPrisms name
  let className = mkName $ "As" <> nameBase name
      prismName = mkName $ "_" <> nameBase name
      where' = valD (varP prismName) (normalB $ varE 'exception) []
  instance' <- instanceD (cxt []) (conT className `appT` conT ''SomeException) [where']
  exceptionOutput <- [d|instance Exception $(conT name)|]
  pure $ classyPrismOutput <> exceptionOutput <> [instance']
