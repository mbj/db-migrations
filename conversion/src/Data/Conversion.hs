{-# LANGUAGE DefaultSignatures #-}

module Data.Conversion where

import Control.Bool (guard')
import Control.Exception (Exception)
import Control.Monad (MonadPlus(..))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Except (MonadError, throwError)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Natural (Natural)
import Prelude hiding (max, min)

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.Lazy             as Text.Lazy
import qualified GHC.Show                   as Show
import qualified Language.Haskell.TH.Syntax as TH

data BoundError a b = (Bounded b, Show a, Show b) => BoundError a

instance Show.Show (BoundError a b) where
  show (BoundError value) = boundError value (minBound @b) (maxBound @b)

instance (Typeable a, Typeable b) => Exception (BoundError a b)

data UserBoundError a b = UserBoundError a b b
  deriving stock (Typeable)

instance (Show a, Show b) => Show.Show (UserBoundError a b) where
  show (UserBoundError value min max) = boundError value min max

instance (Show a, Show b, Typeable a, Typeable b) => Exception (UserBoundError a b)

class Conversion a b where
  convert :: a -> b

  default convert :: (Integral a, Num b) => a -> b
  convert = fromIntegral

class ( Show a, Show b, Typeable a, Typeable b
      , Show (ConversionError a b)
      , Exception (ConversionError a b)
      )
  => PartialConversion a b where

  type ConversionError a b :: Type
  type ConversionError a b = BoundError a b

  convertError :: forall m. (MonadError (ConversionError a b) m) => a -> m b

  default convertError
    :: forall m.
      ( MonadError (BoundError a b) m
      , Bounded b, Num b, Integral a
      , Conversion b a
      )
    => a -> m b
  convertError value =
    if convert (minBound @b) <= value && value <= convert (maxBound @b)
      then pure $ fromIntegral value
      else throwError $ BoundError value

instance PartialConversion Int Natural where
  type ConversionError Int Natural = UserBoundError Int Natural

  convertError = convertErrorFromIntegral

instance PartialConversion Int64 Natural where
  type ConversionError Int64 Natural = UserBoundError Int64 Natural

  convertError = convertErrorFromIntegral

instance PartialConversion Int32 Natural where
  type ConversionError Int32 Natural = UserBoundError Int32 Natural

  convertError = convertErrorFromIntegral

instance PartialConversion Int16 Natural where
  type ConversionError Int16 Natural = UserBoundError Int16 Natural

  convertError = convertErrorFromIntegral

instance PartialConversion Int8 Natural where
  type ConversionError Int8 Natural = UserBoundError Int8 Natural

  convertError = convertErrorFromIntegral

instance PartialConversion Word Natural where
  type ConversionError Word Natural = UserBoundError Word Natural

  convertError = convertErrorFromIntegral

instance PartialConversion Word64 Natural where
  type ConversionError Word64 Natural = UserBoundError Word64 Natural

  convertError = convertErrorFromIntegral

instance PartialConversion Word32 Natural where
  type ConversionError Word32 Natural = UserBoundError Word32 Natural

  convertError = convertErrorFromIntegral

instance PartialConversion Word16 Natural where
  type ConversionError Word16 Natural = UserBoundError Word16 Natural

  convertError = convertErrorFromIntegral

instance PartialConversion Integer Natural where
  type ConversionError Integer Natural = UserBoundError Integer Text

  convertError value
    = maybe (throwError $ UserBoundError value "0" "Natural") pure
    $ checkedFromIntegral value

instance PartialConversion Natural Int where
  type ConversionError Natural Int = UserBoundError Natural Int

  convertError = convertErrorFromNatural

instance PartialConversion Natural Int16 where
  type ConversionError Natural Int16 = UserBoundError Natural Int16

  convertError = convertErrorFromNatural

instance PartialConversion Natural Int32 where
  type ConversionError Natural Int32 = UserBoundError Natural Int32

  convertError = convertErrorFromNatural

instance PartialConversion Natural Int64 where
  type ConversionError Natural Int64 = UserBoundError Natural Int64

  convertError = convertErrorFromNatural

instance Conversion a a where
  convert = id

instance Conversion Int Integer

instance Conversion Word32 Integer
instance Conversion Word16 Integer
instance Conversion Word8  Integer

instance Conversion Word32 Natural
instance Conversion Word16 Natural
instance Conversion Word8  Natural

instance Conversion Natural Integer

instance PartialConversion Integer Int
instance PartialConversion Integer Word32
instance PartialConversion Integer Word16
instance PartialConversion Integer Word8

instance Conversion BS.ByteString LBS.ByteString where
  convert = LBS.fromStrict

instance Conversion LBS.ByteString BS.ByteString where
  convert = LBS.toStrict

instance Conversion Text BS.ByteString where
  convert = Text.encodeUtf8

instance Conversion Text Text.Lazy.Text where
  convert = Text.Lazy.fromStrict

instance Conversion Text.Lazy.Text Text where
  convert = Text.Lazy.toStrict

instance Conversion String Text where
  convert = Text.pack

instance Conversion Text String where
  convert = Text.unpack

convertErrorFromNatural
  :: forall a m. (Integral a, Bounded a, MonadError (UserBoundError Natural a) m)
  => Natural
  -> m a
convertErrorFromNatural value
  = maybe (throwError $ UserBoundError value minBound maxBound) pure
  $ checkedFromIntegral value

convertErrorFromIntegral
  :: forall a m. (Integral a, Bounded a, MonadError (UserBoundError a Natural) m)
  => a
  -> m Natural
convertErrorFromIntegral value
  = maybe (throwError $ UserBoundError value 0 maxBound') pure
  $ checkedFromIntegral value
  where
    maxBound' :: Natural
    maxBound' = fromIntegral $ maxBound @a

checkedFromIntegral
  :: forall a b m. (MonadPlus m, Integral a, Integral b)
  => a
  -> m b
checkedFromIntegral value = guard' (fromIntegral converted == value) converted
  where
    converted :: b
    converted = fromIntegral value

convertUnsafe :: PartialConversion a b => a -> b
convertUnsafe = either (error . show) id . convertError

convertThrow :: (MonadThrow m, PartialConversion a b) => a -> m b
convertThrow = either throwM pure . convertError

convertFail :: (MonadFail m, PartialConversion a b) => a -> m b
convertFail = either (fail . show) pure . convertError

convertMaybe :: PartialConversion a b => a -> Maybe b
convertMaybe = convertThrow

boundError :: forall a b. (Show a, Show b) => a -> b -> b -> String
boundError value min max
  = "Value should be between "
  <> show min
  <> " and "
  <> show max
  <> " but was "
  <> show value

mkTH :: forall a b . (TH.Lift b, PartialConversion a b) => a -> TH.Q (TH.TExp b)
mkTH input = TH.TExp <$> either (fail . show) (TH.lift @b) (convertThrow input)
