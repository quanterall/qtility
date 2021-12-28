module Brick.Widgets.Histogram where

import Brick
import Numeric (showFFloat)
import RIO
import qualified RIO.List as List
import qualified RIO.List.Partial as PartialList
import qualified RIO.Text as Text

newtype FillCharacter = FillCharacter {unFillCharacter :: Char}
  deriving (Eq, Show)

data ScaledValue a = ScaledValue
  { name :: !Text,
    value :: !a,
    scaledValue :: !Float,
    percentage :: !Float
  }
  deriving (Eq, Show)

data HistogramSorting
  = KeyAscending
  | KeyDescending
  | ValueAscending
  | ValueDescending
  | NoSorting
  deriving (Eq, Show)

histogramLineAttr :: AttrName
histogramLineAttr = "histogram-line"

histogramLineKeyAttr :: AttrName
histogramLineKeyAttr = "histogram-line-key"

histogramLineValueAttr :: AttrName
histogramLineValueAttr = "histogram-line-value"

histogramLineFillEvenAttr :: AttrName
histogramLineFillEvenAttr = "histogram-line-fill-even"

histogramLineFillOddAttr :: AttrName
histogramLineFillOddAttr = "histogram-line-fill-odd"

drawHistogram :: (Real a, Show a) => FillCharacter -> HistogramSorting -> [(Text, a)] -> Widget n
drawHistogram _fillCharacter _sorting [] = fill ' '
drawHistogram (FillCharacter fillCharacter) sorting keyValues = do
  let maxValue = keyValues & PartialList.maximumBy (comparing snd) & snd
      total = keyValues & map snd & List.sum
      relativeValues = map (scaleTo total maxValue) keyValues
      longestNameLength =
        -- We know because of the other pattern match that `maximumBy` is safe here
        keyValues & PartialList.maximumBy (comparing (fst >>> Text.length)) & fst & Text.length
      sortValues = case sorting of
        KeyAscending -> List.sortOn name
        ValueAscending -> List.sortOn value
        KeyDescending -> List.sortBy (\a b -> compare (name b) (name a))
        ValueDescending -> List.sortBy (\a b -> compare (value b) (value a))
        NoSorting -> id
  relativeValues & sortValues & mapWithIndex (drawEntry longestNameLength) & vBox
  where
    scaleTo :: (Real a) => a -> a -> (Text, a) -> ScaledValue a
    scaleTo total maxValue (name, value) =
      ScaledValue
        { name,
          value,
          scaledValue = realToFrac value / realToFrac maxValue,
          percentage = (realToFrac value / realToFrac total) * 100
        }

    drawEntry :: (Show a) => Int -> Int -> ScaledValue a -> Widget n
    drawEntry longestNameLength n ScaledValue {name, value, scaledValue, percentage} = do
      let fillAttr = if even n then histogramLineFillEvenAttr else histogramLineFillOddAttr
      Widget Greedy Greedy $ do
        context <- getContext
        let availableWidth = availWidth context
            spaceForFill = availableWidth - longestNameLength - percentageLength - 2
            fillWidth = floor $ scaledValue * fromIntegral spaceForFill
            percentageString = show value <> " (" <> showFFloat (Just 2) percentage "%)"
            percentageLength = length percentageString
        render $
          withAttr histogramLineAttr $
            vLimit 1 $
              hBox
                [ hLimit longestNameLength (withAttr histogramLineKeyAttr $ txt name <+> fill ' '),
                  str " ",
                  hLimit fillWidth $ withAttr fillAttr $ fill fillCharacter,
                  str " ",
                  withAttr histogramLineValueAttr $ str percentageString,
                  fill ' '
                ]

unicodeDot :: FillCharacter
unicodeDot = FillCharacter '•'

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex _f [] = []
mapWithIndex f (x : xs) = f 0 x : mapWithIndex ((+ 1) >>> f) xs
