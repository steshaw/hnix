module Nix.Eval where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad hiding (mapM, sequence)
import           Data.Foldable (foldl')
import           Data.Functor.Compose
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable as T
import           Nix.Types
import           Prelude hiding (mapM, sequence)

buildArgument :: Formals NThunk -> NThunk -> IO (Map.Map Text NThunk)
buildArgument paramSpec arg = case paramSpec of
    FormalName name -> return $ Map.singleton name arg
    FormalSet s -> lookupParamSet s
    FormalLeftAt name s -> Map.insert name arg <$> lookupParamSet s
    FormalRightAt s name -> Map.insert name arg <$> lookupParamSet s
  where
    go env k def = maybe (Left err) Right $ Map.lookup k env <|> def
      where err = "Could not find " ++ show k

    lookupParamSet :: FormalParamSet NThunk -> IO (Map.Map Text NThunk)
    lookupParamSet (FormalParamSet s) = do
      arg' <- whnf arg
      case arg' of
        NVSet args -> either error return $ Map.traverseWithKey (go args) s
        a          -> error $ "Value is a " ++ valueType a ++ " while a set was expected"

evalExpr :: NExpr -> Map.Map Text NThunk -> NThunk
evalExpr = cata phi
  where
    phi :: NExprF (Map.Map Text NThunk -> NThunk) -> Map.Map Text NThunk -> NThunk
    phi (NSym var)
      = fromMaybe (error ("Undefined variable: " ++ show var)) . Map.lookup var
    phi (NConstant x) = const $ delayPure $ Fix $ NVConstant x
    phi (NStr str) = delay . fmap (Fix . NVStr) . flip evalString str
    phi (NOper _x) = error "Operators are not yet defined"
    phi (NSelect _x _attr _or) = error "Select expressions are not yet supported"
    phi (NHasAttr _x _attr) = error "Has attr expressions are not yet supported"

    phi (NList l) = Fix . Compose . return . NVList . sequenceA l
    phi (NSet b binds) = \env -> Fix . Compose . fmap NVSet $ evalBinds True b env binds
    phi (NLet binds e) = \env -> Fix . Compose $
      whnf . e . (`Map.union` env) =<< evalBinds False Rec env binds
    phi (NIf cond t f) = \env -> Fix . Compose $ do
      cval <- whnf $ cond env
      case cval of
        NVConstant (NBool True) -> whnf $ t env
        NVConstant (NBool False) -> whnf $ f env
        x -> error $ "value is a " ++ valueType x ++ " while a boolean was expected"

    phi (NWith scope e) = \env -> Fix . Compose $ do
      s <- whnf $ scope env
      case s of
        (NVSet scope') -> whnf $ e $ Map.union scope' env
        x -> error $ "value is a " ++ valueType x ++ " while a set was expected"

    phi (NAssert cond e) = \env -> Fix . Compose $ do
      cond' <- whnf $ cond env
      case cond' of
        NVConstant (NBool True) -> whnf $ e env
        NVConstant (NBool False) -> error "assertion failed"
        x -> error $ "value is a " ++ valueType x ++ " while a boolean was expected"

    phi (NApp fun x) = \env -> Fix . Compose $ do
      fun' <- whnf $ fun env
      case fun' of
        NVFunction argset f -> whnf . f =<< buildArgument argset (x env)
        _ -> error "Attempt to call non-function"

    phi (NAbs a b) = \env -> Fix . Compose . return $ NVFunction (fmap ($ env) a) $
      b . (`Map.union` env)

evalString :: Map.Map Text NThunk -> NString (Map.Map Text NThunk -> NThunk) -> IO Text
evalString env (NString _ parts)
  = Text.concat <$> mapM (runAntiquoted return (thunkText . ($ env))) parts
evalString _ (NUri t) = return t

thunkText :: NThunk -> IO Text
thunkText t = flip fmap (whnf t) $ \v -> case v of
  NVConstant a -> atomText a
  NVStr text   -> text
  x            -> error $ "Cannot coerce " ++ valueType x ++ " to a string"

data BindValue = BindSet   (Map.Map Text BindValue)
               | BindThunk NThunk

bindValueToThunk :: BindValue -> NThunk
bindValueToThunk (BindSet attrs) = Fix . Compose . return $ NVSet $
  fmap bindValueToThunk attrs
bindValueToThunk (BindThunk thunk) = thunk

bindValue :: Map.Map Text BindValue -> [Text] -> NThunk -> Map.Map Text BindValue
bindValue _ [] _ = error "invalid selector with no components"
bindValue m (p:ps) v = modifyPath ps (insertIfNotMember p $ BindThunk v) where
  alreadyDefinedErr = error $ "attribute " ++ attr ++ " already defined"
  attr = show $ Text.intercalate "." $ reverse (p:ps)

  modifyPath [] f = f m
  modifyPath (key:rest) f = modifyPath rest $ \parent -> case Map.lookup key parent of
        Nothing                -> Map.insert key (BindSet $ f Map.empty) parent
        Just (BindSet current) -> Map.insert key (BindSet $ f current)   parent
        Just _                 -> alreadyDefinedErr

  insertIfNotMember k x m'
    | Map.notMember k m' = Map.insert k x m'
    | otherwise = alreadyDefinedErr

evalBinds :: Bool
          -> NSetBind
          -> Map.Map Text NThunk
          -> [Binding (Map.Map Text NThunk -> NThunk)]
          -> IO (Map.Map Text NThunk)
evalBinds allowDynamic kind env xs = buildResult <$> sequence (concatMap go xs) where
  buildResult :: [([Text], Map.Map Text NThunk -> NThunk)] -> Map.Map Text NThunk
  buildResult binds = result where
    result :: Map.Map Text NThunk
    result = fmap bindValueToThunk . buildMap . map (reverse *** ($ result)) $ binds
    buildMap = foldl' (uncurry . bindValue) Map.empty

  -- TODO: Inherit
  go :: Binding (Map.Map Text NThunk -> NThunk) -> [IO ([Text], Map.Map Text NThunk -> NThunk)]
  go (NamedVar x y) = [fmap (, f) (evalSelector x)] where
    f = case kind of
      Rec    -> \self -> y (Map.union self env)
      NonRec -> \_    -> y env

  evalSelector :: NSelector (Map.Map Text NThunk -> NThunk) -> IO [Text]
  evalSelector = mapM evalKeyName where
    evalKeyName (StaticKey k) = return k
    evalKeyName (DynamicKey k)
      | allowDynamic = runAntiquoted (evalString env) (thunkText . ($ env)) k
      | otherwise    = error "dynamic attribute not allowed in this context"
