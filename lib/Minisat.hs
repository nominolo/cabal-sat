{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Minisat 
  ( MinisatM
  , runMinisatM

  , Lit, Var
  , freshVar, mkLit, negLit

  , addClause
  , solve
  , modelValue
  
  )
where

import Foreign.C.Types       ( CInt(..) )
import Foreign.Ptr           ( Ptr, FunPtr, nullPtr )
import Foreign.C.String      ( CString, withCString )
import Foreign.Storable      ( Storable )
import Foreign.Marshal.Array ( withArray0 )
import Control.Exception     ( finally )
import Control.Applicative
import Control.Monad.IO.Class ( MonadIO(..) )



newtype Solver = Solver (Ptr ())

newtype Var = Var CInt
  deriving (Eq, Ord)

instance Show Var where
  show (Var n) = 'p':show n

newtype Lit = Lit CInt
  deriving (Eq, Num, Ord, Storable)

instance Show Lit where
  show (Lit n) | n > 0     = '+':'p':show (n - 1)
               | n < 0     = '-':'p':show (abs n - 1)
               | otherwise = "undef"

newtype MinisatM a = MinisatM{ unMinisatM :: Solver -> IO a }

instance Functor MinisatM where
  fmap f (MinisatM m) = MinisatM (fmap f . m)

instance Applicative MinisatM where
  pure x = MinisatM (\_ -> return x)
  MinisatM mf <*> MinisatM mg = MinisatM $ \s ->
    mf s <*> mg s

instance Monad MinisatM where
  return = pure
  MinisatM m >>= k = MinisatM $ \s -> do
    a <- m s
    unMinisatM (k a) s

instance MonadIO MinisatM where
  liftIO m = MinisatM $ \_ -> m

runMinisatM :: MinisatM a -> IO a
runMinisatM (MinisatM f) = withSolver f

mkLit :: Var -> Lit
mkLit (Var n) = Lit (n + 1)

negLit :: Lit -> Lit
negLit (Lit n) = Lit (negate n)

freshVar :: MinisatM Var
freshVar = MinisatM $ \s -> cNewVar s 1 1

addClause :: [Lit] -> MinisatM Bool
addClause [l]          = MinisatM $ \s -> fromCBool <$> cAddClause1 s l
addClause [l1, l2]     = MinisatM $ \s -> fromCBool <$> cAddClause2 s l1 l2
addClause [l1, l2, l3] = MinisatM $ \s -> fromCBool <$> cAddClause3 s l1 l2 l3
addClause lits = MinisatM $ \s -> withArray0 (Lit 0) lits $ \ptr ->
                   fromCBool <$> cAddClause s ptr

solve :: Bool -> [Lit] -> MinisatM Bool
solve simpl assumptions = MinisatM $ \s ->
  withArray0 (Lit 0) assumptions $ \ptr ->
    fromCBool <$> cSolve s (toCBool simpl) ptr

withSolver :: (Solver -> IO a) -> IO a
withSolver act = do
  solver <- cNewSolver
  a <- act solver `finally` cDeleteSolver solver
  return a

main = do
  runMinisatM $ do
    x <- mkLit <$> freshVar
    y <- mkLit <$> freshVar
    liftIO . print =<< addClause [x, y]
    liftIO . print =<< solve True [x, y]
    liftIO . print =<< solve True [negLit x, negLit y]

fromCBool :: CInt -> Bool
fromCBool n = n /= 0

toCBool :: Bool -> CInt
toCBool True = 1
toCBool False = 0

fromLBool :: CInt -> Maybe Bool
fromLBool 0 = Just True
fromLBool 1 = Just False
fromLBool _ = Nothing

  -- | n == 0 = Just True
  -- | n > 0  = Just True
  -- | n < 0  = Just False

modelValue :: Var -> MinisatM (Maybe Bool)
modelValue v = MinisatM $ \s -> do
  --liftIO $ putStrLn $ "modelValue(" ++ show v ++ ") = ?"
  b <- cModelValue s v
  --liftIO $ putStrLn $ "modelValue(" ++ show v ++ ") = " ++ show b
  return (fromLBool b)

foreign import ccall "static wrapper.h minisat_new_solver" cNewSolver :: IO Solver
foreign import ccall "static wrapper.h minisat_delete_solver" cDeleteSolver :: Solver -> IO ()

foreign import ccall unsafe "static wrapper.h minisat_new_var" cNewVar :: Solver -> CInt -> CInt -> IO Var
foreign import ccall unsafe "static wrapper.h minisat_add_clause1" cAddClause1 :: Solver -> Lit -> IO CInt
foreign import ccall unsafe "static wrapper.h minisat_add_clause2" cAddClause2 :: Solver -> Lit -> Lit -> IO CInt
foreign import ccall unsafe "static wrapper.h minisat_add_clause3" cAddClause3 :: Solver -> Lit -> Lit -> Lit -> IO CInt
foreign import ccall unsafe "static wrapper.h minisat_add_clause"  cAddClause :: Solver -> Ptr Lit -> IO CInt

foreign import ccall unsafe "static wrapper.h minisat_model_value"  cModelValue :: Solver -> Var -> IO CInt

foreign import ccall "static wrapper.h minisat_solve" cSolve :: Solver -> CInt -> Ptr Lit -> IO CInt

