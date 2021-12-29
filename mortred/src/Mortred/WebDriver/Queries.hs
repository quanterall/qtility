-- | Holds queries for 'Element' objects from "Test.WebDriver". These can be used to filter and
-- otherwise interact with the elements on a Haskell level.
module Mortred.WebDriver.Queries where

import RIO
import qualified RIO.Text as Text
import Test.WebDriver

-- | @'attributeMatches' attributeName predicate element@ tests a predicate against a the value of
-- the attribute pulled out of the element. If the element does not have the attribute, always
-- returns 'False'.
attributeMatches :: Text -> (Text -> Bool) -> Element -> WD Bool
attributeMatches attribute p element = do
  maybe False p <$> element `attr` attribute

-- | @'classMatches' predicate element@ checks a predicate against the value of the element's class
-- attribute value. If the element does not have a class attribute it returns 'False'.
classMatches :: (Text -> Bool) -> Element -> WD Bool
classMatches = attributeMatches "class"

-- | @'classContainsAny' classes element@ checks whether or not any of the classes passed are
-- matched partially in the class(es) of @element@. Returns 'False' if there are no classes.
classContainsAny :: [Text] -> Element -> WD Bool
classContainsAny classes =
  classMatches (\class' -> any (`Text.isInfixOf` class') classes)

-- | Flipped version of 'findElemsFrom', allowing one to more descriptively write code as follows:
--
-- > (ByClass "jobTitle") `manyInsideOf` e
manyInsideOf :: Selector -> Element -> WD [Element]
manyInsideOf = flip findElemsFrom

-- | Flipped version of 'findElemFrom', allowing one to more descriptively write code as follows:
--
-- > (ByID "jobTitle") `oneInsideOf` e
oneInsideOf :: Selector -> Element -> WD Element
oneInsideOf = flip findElemFrom
