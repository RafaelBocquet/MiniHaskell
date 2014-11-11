module Core.Pretty where

import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer

import Core.Expression
import Core.Module

import qualified Data.Map as Map

type PrettyMonad a = WriterT String (Reader Int) a

runPretty :: PrettyMonad () -> String
runPretty = flip runReader 0 . execWriterT

indent :: PrettyMonad a -> PrettyMonad a
indent = local (+ 1)

pretty :: PrettyMonad () -> PrettyMonad ()
pretty m = do
  tell "\n"
  ni <- ask
  forM [1..ni] $ const (tell "\t")
  m

prettyCoreModule (Module md _ ds) = do
  tell (concat md)
  forM (Map.toList ds) $ \(v, (ty, e')) -> indent $ pretty $ case e' of
    Declaration e -> do
      tell $ show v
      tell " :: "
      tell $ show ty
      pretty $ do
        tell $ show v
        tell " = "
        prettyCoreExpression e
    _ -> tell "PRIMITIVE"

prettyCoreExpression (Expression _ e) = prettyCoreExpression' e

prettyCoreExpression' :: Expression' -> PrettyMonad ()
prettyCoreExpression' (EInteger i)       = tell $ show i
prettyCoreExpression' (EChar c)          = tell $ show c
prettyCoreExpression' (EVariable v)      = tell $ show v
prettyCoreExpression' (EApplication f t) = do
  tell "("
  prettyCoreExpression f
  tell " "
  prettyCoreExpression t
  tell ")"
prettyCoreExpression' (ELambda v e)      = do
  tell "(λ"
  tell $ show v
  tell " -> "
  prettyCoreExpression e
  tell ")"
prettyCoreExpression' (ELet bs e)        = do
  tell "let "
  forM (Map.toList bs) $ \(v, (_, e')) -> indent $ pretty $ do
    tell $ show v
    tell " = "
    prettyCoreExpression e'
  pretty $ do
    tell "in "
    prettyCoreExpression e
prettyCoreExpression' (ECase e pats)     = do
  tell "case "
  prettyCoreExpression e
  pretty $ tell " of"
  indent $ prettyPatternGroup pats

prettyPatternGroup (PData ps df) = do
  forM (Map.toList ps) $ \(con, (vs, e)) -> pretty $ do
    tell "("
    tell $ show con
    forM vs $ \v -> tell $ " " ++ show v
    tell ") -> "
    indent $ pretty $ prettyCoreExpression e
  maybe (return ()) (\df -> pretty $ do
                        tell "_ -> "
                        prettyCoreExpression df
                    ) df
