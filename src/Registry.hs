

module Registry where

import qualified Data.Vector.Mutable as Vector
import Foreign.Ptr
import Memory.ArrayBuffer(ArrayBuffer(..))
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

lifetimeOneDrawRenderers :: IORef (HashTable Int ( RenderVertFrag, RenderDataProvider) )
{-# NOINLINE lifetimeOneDrawRenderers #-}
lifetimeOneDrawRenderers =
  unsafePerformIO $ do
    table <- H.new
    newIORef table

currentRenderId :: IORef Int
{-# NOINLINE currentRenderId #-}
currentRenderId = unsafePerformIO (newIORef 0)

data RenderLifetime = RenderLifetimeOneDraw | RenderLifetimeManual
data RenderTranformation = RenderTransformationNone | RenderTransformationUI | RenderTransformationWorld

data WindowInfo = WindowInfo{windowId :: Ptr (), windowWidth :: Int, windowHeight :: Int}

data Pack = Pack {packName :: String, packVersion :: String, packInit :: Registry -> IO (), packDeinit :: Registry -> IO ()}

data PackData = PackData{dataPacks :: ArrayBuffer Int Pack, dataKeyCallbacks :: ArrayBuffer Int GLFWkeyfun, dataMouseCallbacks :: ArrayBuffer Int GLFWmousebuttonfun}

type ApplyShaderData = Shader -> WindowInfo -> IO ()
type ApplyPreRenderState = IO ()
type ApplyPostRenderState = IO ()

data RenderDataProvider = RenderDataProvider {applyShaderData :: Maybe ApplyShaderData, applyPreRenderState :: Maybe ApplyPreRenderState, applyPostRenderState :: Maybe ApplyPostRenderState}

data Registry = Registry {
  addPack :: Pack -> IO (),
  addKeyCallback :: GLFWkeyfun -> IO (),
  addMouseCallback :: GLFWmousebuttonfun -> IO ()
}

data Render = Render{
  push :: RenderLifetime -> RenderTranformation -> RenderVertFrag -> Maybe RenderDataProvider -> IO Int
}

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


_pushImpl :: RenderLifetime -> RenderTranformation -> RenderVertFrag -> Maybe RenderDataProvider -> IO Int
_pushImpl lifetime transform render maybeProvider =
  case lifetime of
    RenderLifetimeOneDraw -> proceed
    RenderLifetimeManual  -> proceed
    _                     -> throw UnsupportedRenderLifetimeException
  where
    defaultProvider shader widnowInfo = pure () --default provider
    proceed :: IO Int
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


      lcombinedProvider <- case transform of
        RenderTransformationUI -> pure providerByUser --TODO implement
        RenderTransformationNone -> pure providerByUser
        _ -> throw UnsupportedRenderTransformationException

      id <- (+1) <$> readIORef currentRenderId
      writeIORef currentRenderId id

      pre <- readIORef lapplyPreRenderState
      post <- readIORef lapplyPostRenderState

      let t :: Maybe ApplyShaderData
          t = Just lcombinedProvider

      let newProvider = RenderDataProvider (Just lcombinedProvider) pre post

      case lifetime of
        RenderLifetimeOneDraw -> pure () --TODO continue here, lifetimeOneDrawRenderers should be mutable hashmap !!! not mutable reference to immutable hashmap !!!
        RenderLifetimeManual -> pure () --TODO implement
        _ -> pure ()

      pure id
