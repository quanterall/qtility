module Qtility.TH where

import Control.Lens.TH (makeLenses)
import Language.Haskell.TH
import Qtility.TH.JSON (deriveJSON)
import RIO

deriveLensAndJSON :: Name -> Q [Dec]
deriveLensAndJSON name = do
  lenses <- makeLenses name
  (lenses <>) <$> deriveJSON name
