module Compiler.TypeSystem.Type.Infer.Literal where


import Compiler.Counter ( fresh )

import Compiler.Syntax.Kind ( Kind(K'Star) )
import Compiler.Syntax.Literal ( Literal(..) )
import Compiler.Syntax.Predicate ( Predicate(..) )
import Compiler.Syntax.Type ( Rho'Type, Sigma'Type, Type(T'Var', T'Forall, T'Meta, T'Con), T'C(..), T'V'(T'V'), M'V(..) )
import Compiler.Syntax.Qualified ( Qualified(..) )


import Compiler.TypeSystem.Infer ( Infer, Type'Check, add'constraints, get'constraints )
import Compiler.TypeSystem.Type.Constants ( t'Char )
import Compiler.TypeSystem.Expected ( Expected(Check, Infer) )
import Compiler.TypeSystem.Actual ( Actual )
import Compiler.TypeSystem.Constraint ( Constraint )
import Compiler.TypeSystem.Error ( Error )
import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Solver.Substitution ( Subst(..) )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply) )
import Compiler.TypeSystem.Utils.Infer ( inst'sigma )


-- the future implementation will be calling inst'sigma because of the checking mode
-- and it seems to me, that what if I have something like this:
-- 23 :: Int
-- that leads to the checking mode and literal 23 is checked against the Int
-- so inst'sigma is called with the type of the numeric literal
-- which is forall a . Num a => a
-- and the expected type is Int
-- inst'sigma in checking mode does calls subs'check'rho
-- 
infer'lit :: Literal -> Expected Rho'Type -> Type'Check (Literal, [Predicate], Actual Rho'Type)
infer'lit (Lit'Int int) expected = do
  fresh'name <- fresh -- not necessary, since it will get instantiated anyway, but just to be sure
  let t'v = T'V' fresh'name K'Star
      t'var = T'Var' t'v
      sigma :: Sigma'Type
      sigma = T'Forall [t'v] ([Is'In "Num" t'var] :=> t'var)
  (preds, actual) <- inst'sigma sigma expected
  -- an integer literal checked against Double is a Double at runtime
  lit' <- case expected of
    Check rho -> do
      constraints <- get'constraints
      case run'solve constraints :: Either Error (Subst M'V Type) of
        Right subst -> case apply subst rho of
          T'Con (T'C "Double" _) -> return $ Lit'Double (fromIntegral int)
          _ -> return $ Lit'Int int
        Left _ -> return $ Lit'Int int
    Infer -> return $ Lit'Int int
  return (lit', preds, actual)

infer'lit (Lit'Double double) expected = do
  fresh'name <- fresh -- not necessary, since it will get instantiated anyway, but just to be sure
  let t'v = T'V' fresh'name K'Star
      t'var = T'Var' t'v
      sigma :: Sigma'Type
      sigma = T'Forall [t'v] ([Is'In "Fractional" t'var] :=> t'var)
  (preds, actual) <- inst'sigma sigma expected
  return (Lit'Double double, preds, actual)

infer'lit (Lit'Char char) expected = do
  (preds, actual) <- inst'sigma t'Char expected
  return (Lit'Char char, preds, actual)
  