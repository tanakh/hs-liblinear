{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
module Data.Liblinear (
  Problem(..),
  Sample(..),
  Feature(..),
  Parameter(..),
  train,
  def,
  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM

import Control.Applicative
import Data.Default
import Data.Liblinear.Internal
import Foreign.C
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Generic
import GHC.Generics
import System.IO

data Problem
  = Problem
    { problemData :: V.Vector Sample
    , problemBias :: Double
    }

data Sample
  = Sample
    { sampleLabel    :: {-# UNPACK #-} !CDouble
    , sampleFeatures :: {-# UNPACK #-} !(VS.Vector Feature)
    }

data Feature
  = Feature
    { featureIndex :: {-# UNPACK #-} !CInt
    , featureValue :: {-# UNPACK #-} !CDouble
    }
    deriving (Generic)

data Parameter
  = Parameter
    { paramSolverType :: Int
    , paramEps :: Double
    , paramC :: Double
    , paramWeights :: VS.Vector Weight
    , paramP :: Double
    }

instance Default Parameter where
  def = Parameter
    { paramSolverType = c'L2R_L2LOSS_SVC_DUAL
    , paramEps = 0.1
    , paramC = 1
    , paramWeights = VS.empty
    , paramP = 0.1
    }

data Weight
  = Weight
    { weightLabel :: {-# UNPACK #-} !CInt
    , weightValue :: {-# UNPACK #-} !CDouble
    }
    deriving (Generic)

instance Storable Feature where
  sizeOf = sizeOfDefault
  alignment = alignmentDefault
  peek = peekDefault
  poke = pokeDefault

instance Storable Weight where
  sizeOf = sizeOfDefault
  alignment = alignmentDefault
  peek = peekDefault
  poke = pokeDefault

newtype Model = Model { unModel :: Ptr C'model }

train :: Problem -> Parameter -> IO Model
train Problem {..} Parameter {..} = do
  let l = fromIntegral . V.length $ problemData
      n = fromIntegral . V.foldl max 0 . V.map (VS.foldl max 0 . VS.map featureIndex . sampleFeatures) $ problemData
  VS.unsafeWith (V.convert $ V.map sampleLabel problemData) $ \plabels -> do
  withManyV VS.unsafeWith (V.map sampleFeatures problemData) $ \pfeatures -> do
  VS.unsafeWith (VS.map weightLabel paramWeights) $ \pwlabel -> do
  VS.unsafeWith (VS.map weightValue paramWeights) $ \pwvalue -> do
  with (C'problem l n plabels (castPtr pfeatures) (realToFrac problemBias)) $ \pprob -> do
  with (C'parameter (fromIntegral paramSolverType) (realToFrac paramEps) (realToFrac paramC) (fromIntegral $ VS.length paramWeights) pwlabel pwvalue (realToFrac paramP)) $ \pparam -> do
    Model <$> c'train pprob pparam

withManyV :: (Storable e)
          => (VS.Vector e -> (Ptr e -> IO res) -> IO res)
          -> V.Vector (VS.Vector e)
          -> (Ptr (Ptr e) -> IO res)
          -> IO res
withManyV withFoo v f = go 0 =<< VSM.new n where
  n = V.length v
  go ix w
    | ix == n = VSM.unsafeWith w f
    | otherwise = do
      withFoo (v V.! ix) $ \ptr -> do
        VSM.write w ix ptr
        go (ix + 1) w
