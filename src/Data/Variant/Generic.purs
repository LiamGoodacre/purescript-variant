module Data.Variant.Generic where

import Data.Generic.Rep
import Prelude ((<<<), (#), (>>>), show, id)
import Data.Variant (Variant, inj, case_, on)
import Data.Lens (Iso', iso, view, re)
import Type.Data.Symbol (SProxy(..), class IsSymbol)
import Unsafe.Coerce (unsafeCoerce)

rep :: forall dt rep. Generic dt rep => Iso' dt rep
rep = iso from to

class VariantIso (dt :: Type)
                 (out :: Type)
                 | dt -> out where
  variantIso :: Iso' dt out

instance variantIsoInstance
  :: ( Generic dt rep
     , VariantIsoRep rep out )
  => VariantIso dt (Variant out) where
  variantIso = rep <<< variantIsoRep

class VariantIsoRep (rep :: Type)
                    (out :: # Type)
                    | rep -> out where
  variantIsoRep :: Iso' rep (Variant out)

instance variantIsoRepCtorArg
  :: ( IsSymbol ct
     , RowCons ct arg () out )
  => VariantIsoRep (Constructor ct (Argument arg)) out where
  variantIsoRep = iso toVar fromVar where
    ct = SProxy :: SProxy ct
    toVar (Constructor (Argument arg)) = inj ct arg
    fromVar = case_ # on ct (Constructor <<< Argument)

-- not safe in general, but should be fine for constructor names
expand :: forall c t r o.
  RowCons c t r o =>
  SProxy c ->
  Variant r ->
  Variant o
expand _ = unsafeCoerce

instance variantIsoRepSum
  :: ( IsSymbol ct
     , VariantIsoRep r rout
     , RowCons ct arg rout out
     )
  => VariantIsoRep (Sum (Constructor ct (Argument arg)) r) out where
  variantIsoRep = iso toVar fromVar where
    ct = SProxy :: SProxy ct
    isoR = variantIsoRep
    toVar :: Sum (Constructor ct (Argument arg)) r -> Variant out
    toVar (Inl (Constructor (Argument arg))) = inj ct arg
    toVar (Inr r) = expand ct (view isoR r)
    fromVar :: Variant out -> Sum (Constructor ct (Argument arg)) r
    fromVar = on ct (Inl <<< Constructor <<< Argument)
                    (Inr <<< view (re isoR))

--- example

data M = X Int | Y String | Z Boolean
derive instance genericM :: Generic M _

variantM :: Iso' M _
variantM = variantIso

_X = SProxy :: SProxy "X"
_Y = SProxy :: SProxy "Y"
_Z = SProxy :: SProxy "Z"

foo :: M -> String
foo = view variantM >>> (case_
  # on _X show
  # on _Y id
  # on _Z show)

