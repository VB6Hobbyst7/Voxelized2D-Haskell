{-#LANGUAGE TypeApplications#-}

module Registry where

import qualified Data.Vector.Mutable as Vector
import Foreign.Ptr
import Memory.ArrayBuffer(ArrayBuffer(..))
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

--TODO not used anymore
declareIORef "currentRenderId"
  [t|Int|]
  [e|0|]


data RenderLifetime = RenderLifetimeOneDraw | RenderLifetimeManual
data RenderTranformation = RenderTransformationNone | RenderTransformationUI | RenderTransformationWorld

data WindowInfo = WindowInfo{windowId :: Ptr (), windowWidth :: Int, windowHeight :: Int}

data Pack = Pack {packName :: String, packVersion :: String, packInit :: Registry -> IORef WindowInfo -> IO (), packDeinit :: Registry -> IORef WindowInfo -> IO ()}

data PackData = PackData{dataPacks :: ArrayBuffer Int Pack, dataKeyCallbacks :: ArrayBuffer Int GLFWkeyfun, dataMouseCallbacks :: ArrayBuffer Int GLFWmousebuttonfun, dataUpdateCallbacks :: ArrayBuffer Int (IORef WindowInfo -> IO ())}

type ApplyShaderData = Shader -> WindowInfo -> IO ()
type ApplyPreRenderState = IO ()
type ApplyPostRenderState = IO ()

data RenderDataProvider = RenderDataProvider {
  applyShaderData :: Maybe ApplyShaderData,
  applyPreRenderState :: Maybe ApplyPreRenderState,
  applyPostRenderState :: Maybe ApplyPostRenderState
}



data Registry = Registry {
  addPack :: Pack -> IO (),
  addKeyCallback :: GLFWkeyfun -> IO (),
  addMouseCallback :: GLFWmousebuttonfun -> IO (),
  addUpdateCallback :: (IORef WindowInfo -> IO ()) -> IO ()
}

data Render = Render{
  push :: RenderLifetime -> RenderTranformation -> RenderVertFrag -> Maybe RenderDataProvider -> IO ()
}


declareIORef "lifetimeOneDrawRenderers"
  [t| Maybe (ArrayBuffer Int (RenderVertFrag, RenderDataProvider))|]
  [e| Nothing|]

declareIORef "lifetimeManualRenderers"
  [t|Maybe (ArrayBuffer Int (RenderVertFrag, RenderDataProvider))|]
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


_pushImpl :: RenderLifetime -> RenderTranformation -> RenderVertFrag -> Maybe RenderDataProvider -> IO ()
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
          lapplyPreRenderState.>writeIORef $ provider.>applyPreRenderState
          lapplyPostRenderState.>writeIORef $ provider.>applyPostRenderState

          case provider.>applyShaderData of
            (Just applicable) -> pure applicable
            Nothing -> pure defaultProvider
        Nothing -> pure defaultProvider

      lcombinedProvider <- case transform of
        RenderTransformationUI ->
          pure $ \shader win -> do
            (shader.>SU.setMat4) "P" (ortho 0 (fromIntegral $ win.>windowWidth) (fromIntegral $ win.>windowHeight) 0 (-1) 1 ) False
            (shader.>SU.setMat4) "V" (identity (SNat @4)) False

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
          (oneDraw.>ArrayBuffer.push)  (render,newProvider)

          pure ()
        RenderLifetimeManual -> do
          (Just manual) <- readIORef lifetimeManualRenderers
          (manual.>ArrayBuffer.push) (render,newProvider)

          pure ()
          
      pure ()
