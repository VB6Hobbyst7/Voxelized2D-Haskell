{-# LANGUAGE FlexibleContexts, TemplateHaskell, TupleSections #-}

module InlineDo where


import Data.Data (Data, gmapM)
import Data.Generics.Aliases (extM)
import Language.Haskell.TH (Exp(DoE,AppE,VarE,InfixE),Stmt(BindS),Pat(VarP))
import Language.Haskell.TH.Syntax (Quasi,qNewName)

import Control.Monad.Trans.Writer.Strict (runWriterT,WriterT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Class (MonadWriter, tell, pass, listen)


_do :: m a -> a
_do _ = error "pre must be used only inside mkInlineDoBind"

do_ :: (Data a, Quasi m) => m a -> m a
do_ decsQ =
  do
    decs <- decsQ
    (newDecs, leftovers) <- runWriterT (handle decs)
    if not (null leftovers)
      then fail "must use pre only inside do"
      else pure newDecs
  where
    handle :: (Data a, Quasi m) => a -> WriterT [Stmt] m a
    handle = extM (gmapM handle) handleExp
    handleExp :: Quasi m => Exp -> WriterT [Stmt] m Exp
    handleExp (DoE sts) = DoE <$> fmap concat (mapM handleDoSt sts)
    handleExp (AppE func e) | func == VarE '_do = do
      e1 <- handle e
      varName <- lift (qNewName "inlineBound")
      tell [BindS (VarP varName) e1]
      pure (VarE varName)
    handleExp (InfixE (Just func) op (Just e)) | func == (VarE '_do) && op == (VarE '($)) = handleExp (AppE func e)
    handleExp e = gmapM handle e
    handleDoSt st = do
      (st1, binds) <- pass $ fmap (,const []) $ listen (handle st)
      pure (binds ++ [st1])
