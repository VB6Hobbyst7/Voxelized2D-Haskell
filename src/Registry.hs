{-#LANGUAGE TypeApplications#-}
{-# LANGUAGE TemplateHaskell #-}

module Registry where

import Memory.ArrayBuffer(ArrayBuffer)
import qualified Memory.ArrayBuffer as ArrayBuffer
import Graphics.OpenGL(GLFWkeyfun, GLFWmousebuttonfun)
import qualified Data.HashTable.IO as H
import Graphics.Shader.ShaderUtils(Shader(..))
import qualified Graphics.Shader.ShaderUtils as SU
import Data.Maybe
import Graphics.Render.RenderVertFrag(RenderVertFrag(..), RenderVertFragDataDefault(..))
import Data.IORef
import System.IO.Unsafe
import Control.Exception
import Data.Typeable
import Common
import Math.Linear.Mat
import qualified Math.Nat as Nat
import Data.Singletons.TypeLits
import Data.Global
import WindowInfo

import Control.Lens hiding (element)

--TODO not used anymore
declareIORef "currentRenderId"
  [t|Int|]
  [e|0|]


data RenderLifetime = RenderLifetimeOneDraw | RenderLifetimeManual
data RenderTransformation = RenderTransformationNone | RenderTransformationUI | RenderTransformationWorld

data Registry = Registry {
  _addPack :: Pack -> IO (),
  _addKeyCallback :: GLFWkeyfun -> IO (),
  _addMouseCallback :: GLFWmousebuttonfun -> IO (),
  _addUpdateCallback :: (IORef WindowInfo -> IO ()) -> IO ()
}



data Pack = Pack {_name :: String, _version :: String, _init :: Registry -> IORef WindowInfo -> IO (), _deinit :: Registry -> IORef WindowInfo -> IO ()}

data PackData = PackData{_packs :: ArrayBuffer Pack, _keyCallbacks :: ArrayBuffer GLFWkeyfun, _mouseCallbacks :: ArrayBuffer GLFWmousebuttonfun, _updateCallbacks :: ArrayBuffer (IORef WindowInfo -> IO ())}

makeLenses ''Pack
makeLenses ''PackData
makeLenses ''Registry

type ApplyShaderData = Shader -> WindowInfo -> IO ()
type ApplyPreRenderState = IO ()
type ApplyPostRenderState = IO ()

data RenderDataProvider = RenderDataProvider {
  _applyShaderData :: Maybe ApplyShaderData,
  _applyPreRenderState :: Maybe ApplyPreRenderState,
  _applyPostRenderState :: Maybe ApplyPostRenderState
}

makeLenses ''RenderDataProvider



data Render = Render{
  push :: RenderLifetime -> RenderTransformation -> RenderVertFrag -> Maybe RenderDataProvider -> IO ()
}


declareIORef "lifetimeOneDrawRenderers"
  [t| Maybe (ArrayBuffer (RenderVertFrag, RenderDataProvider))|]
  [e| Nothing|]

declareIORef "lifetimeManualRenderers"
  [t|Maybe (ArrayBuffer (RenderVertFrag, RenderDataProvider))|]
  [e|Nothing|]

renderer :: Render
renderer = Render _pushImpl

data UnsupportedRenderLifetimeException = UnsupportedRenderLifetimeException
    deriving Typeable
instance Show UnsupportedRenderLifetimeException where
    show UnsupportedRenderLifetimeException = "Unsupported render lifetime"
instance Exception UnsupportedRenderLifetimeException

data UnsupportedRenderTransformationException = UnsupportedRenderTransformationException
    deriving Typeable
instance Show UnsupportedRenderTransformationException where
    show UnsupportedRenderTransformationException = "Unsupported render transformation"
instance Exception UnsupportedRenderTransformationException


_pushImpl :: RenderLifetime -> RenderTransformation -> RenderVertFrag -> Maybe RenderDataProvider -> IO ()
_pushImpl lifetime transform render maybeProvider =
  case lifetime of
    RenderLifetimeOneDraw -> proceed
    RenderLifetimeManual  -> proceed
    _                     -> throw UnsupportedRenderLifetimeException
  where
    
    defaultProvider shader widnowInfo = pure () --default provider
    proceed :: IO ()
    proceed = do
      lapplyPreRenderState <- newIORef (Nothing :: Maybe ApplyPreRenderState)
      lapplyPostRenderState <- newIORef (Nothing :: Maybe ApplyPostRenderState)

      providerByUser <- case maybeProvider of
        (Just provider) -> do
          writeIORef lapplyPreRenderState ({-RenderDataProvider-}provider^.applyPreRenderState)
          writeIORef lapplyPostRenderState ( {-RenderDataProvider-}provider^.applyPostRenderState)

          case {-RenderDataProvider-}provider^.applyShaderData of
            (Just applicable) -> pure applicable
            Nothing -> pure defaultProvider
        Nothing -> pure defaultProvider

      lcombinedProvider <- case transform of
        RenderTransformationUI ->
          pure $ \shader win -> do
            SU.setMat4 shader "P" (ortho 0 (fromIntegral $ win^.width) (fromIntegral $ win^.height) 0 (-1) 1 ) False
            SU.setMat4 shader "V" (identity (SNat @4)) False

        RenderTransformationNone -> pure providerByUser
        _ -> throw UnsupportedRenderTransformationException

      --id <- (+1) <$> readIORef currentRenderId
      --writeIORef currentRenderId id

      pre <- readIORef lapplyPreRenderState
      post <- readIORef lapplyPostRenderState

      let t :: Maybe ApplyShaderData
          t = Just lcombinedProvider

      let newProvider = RenderDataProvider (Just lcombinedProvider) pre post


      --try change Hashmap to regular IO Vector
      case lifetime of
        RenderLifetimeOneDraw -> do
          (Just oneDraw) <- readIORef lifetimeOneDrawRenderers
          ArrayBuffer.push oneDraw  (render,newProvider)

          pure ()
        RenderLifetimeManual -> do
          (Just manual) <- readIORef lifetimeManualRenderers
          ArrayBuffer.push manual (render,newProvider)

          pure ()
          
      pure ()
