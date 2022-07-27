{-# OPTIONS_GHC -Wno-missing-fields #-}

module RecUpd.QQ (upd, (&)) where

import Control.Monad ( forM )
import Language.Haskell.Meta.Parse ( parseExp )
import Language.Haskell.TH
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )

import Data.Function ( (&) )

import Internal.Parser ( Field, splitFields )

upd :: QuasiQuoter
upd = QuasiQuoter { quoteExp  = either fail upd' . splitFields }

upd' :: [Field] -> Q Exp
upd' fields = do
  let mustLookupValueName s = maybe (fail $ "Identifier '" ++ s ++ "' not found") pure =<< lookupValueName s
      mustParseExp s = either (fail . ("Can't parse expression: " ++)) pure $ parseExp s

  record <- newName "record"
  kv' <- forM fields $ \(k, b, v) -> do
    k' <- mustLookupValueName k
    v' <- mustParseExp v
    pure (k', if b then v' `AppE` (VarE k' `AppE` VarE record) else v')
  pure $ LamE [VarP record] (RecUpdE (VarE record) kv')
