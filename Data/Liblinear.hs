{-# LANGUAGE RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}
module Data.Liblinear (
  Problem(..),
  Example(..),
  Feature(..),
  Parameter(..),
  Model,
  train,
  def,
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Data
import Data.Default
import Data.Monoid
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Generic
import GHC.Generics
import System.IO

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM

import Data.Liblinear.Internal

data Problem
  = Problem
    { problemData :: V.Vector Example
    , problemBias :: Double
    }

data Example
  = Example
    { exampleLabel    :: {-# UNPACK #-} !CDouble
    , exampleFeatures :: {-# UNPACK #-} !(VS.Vector Feature)
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
      n = fromIntegral . V.foldl max 0 . V.map (VS.foldl max 0 . VS.map featureIndex . exampleFeatures) $ problemData
  VS.unsafeWith (V.convert $ V.map exampleLabel problemData) $ \plabels ->
    withManyV VS.unsafeWith (V.map exampleFeatures problemData) $ \pfeatures ->
    VS.unsafeWith (VS.map weightLabel paramWeights) $ \pwlabel ->
    VS.unsafeWith (VS.map weightValue paramWeights) $ \pwvalue ->
    with (C'problem l n plabels (castPtr pfeatures) (realToFrac problemBias)) $ \pprob ->
    with (C'parameter (fromIntegral paramSolverType) (realToFrac paramEps) (realToFrac paramC) (fromIntegral $ VS.length paramWeights) pwlabel pwvalue (realToFrac paramP)) $ \pparam ->
    Model <$> c'train pprob pparam

predict :: Model -> VS.Vector Feature -> IO Double
predict (Model pmodel) features =
  VS.unsafeWith (features <> VS.singleton (Feature (-1) 0)) $ \pfeat ->
  realToFrac <$> c'predict pmodel (castPtr pfeat)

predictValues :: Model -> VS.Vector Feature -> IO (Double, VS.Vector Double)
predictValues (Model pmodel) features = do
  nr_class <- peek $ p'model'nr_class pmodel
  solver_type <- peek $ p'parameter'solver_type $ p'model'param pmodel
  let nr_w | nr_class == 2 && solver_type /= c'MCSVM_CS = 1
           | otherwise = nr_class
  ptr <- mallocForeignPtrArray (fromIntegral nr_w)
  VS.unsafeWith (features <> VS.singleton (Feature (-1) 0)) $ \pfeat -> do
    ret <- withForeignPtr ptr $ c'predict_values pmodel (castPtr pfeat)
    let vect = VS.unsafeFromForeignPtr0 (castForeignPtr ptr) $ fromIntegral nr_w
    return (realToFrac ret, vect)

predictProbability :: Model -> VS.Vector Feature -> IO (Double, VS.Vector Double)
predictProbability (Model pmodel) features = do
  nr_class <- peek $ p'model'nr_class pmodel
  ptr <- mallocForeignPtrArray (fromIntegral nr_class)
  VS.unsafeWith (features <> VS.singleton (Feature (-1) 0)) $ \pfeat -> do
    ret <- withForeignPtr ptr $ c'predict_probability pmodel (castPtr pfeat)
    let vect = VS.unsafeFromForeignPtr0 (castForeignPtr ptr) $ fromIntegral nr_class
    return (realToFrac ret, vect)

saveModel :: FilePath -> Model -> IO ()
saveModel path (Model pmodel) =  do
  withCString path $ \ppath -> do
    throwIfError "saveModel failed" $ c'save_model ppath pmodel

loadModel :: FilePath -> IO Model
loadModel path = do
  withCString path $ \ppath -> do
    Model <$> throwIfNull "loadModel failed" (c'load_model ppath)

crossValidation :: Problem -> Parameter -> Int -> IO [Double]
crossValidation (Problem prob) (Parameter param) numFold = do
  allocaArray foldNum $ \ptr -> do
    c'cross_validation prob param (fromIntegral foldNum) ptr
    map realToFrac <$> peekArray numFold ptr

{-
int get_nr_feature(const struct model *model_);
int get_nr_class(const struct model *model_);
void get_labels(const struct model *model_, int* label);

void free_model_content(struct model *model_ptr);
void free_and_destroy_model(struct model **model_ptr_ptr);
void destroy_param(struct parameter *param);

const char *check_parameter(const struct problem *prob, const struct parameter *param);
int check_probability_model(const struct model *model);
void set_print_string_function(void (*print_func) (const char*));
-}

--

data LiblinearError
  = LiblinearError String
  deriving (Show, Data, Typeable)

instance Exception LiblinearError

throwIfError :: String -> IO CInt -> IO ()
throwIfError msg m = do
  c <- m
  when (c /= 0) $ throwIO $ LiblinearError msg

throwIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwIfNull msg m = do
  ptr <- m
  when (ptr == nullPtr) $ throwIO $ LiblinearError msg
  return ptr

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
