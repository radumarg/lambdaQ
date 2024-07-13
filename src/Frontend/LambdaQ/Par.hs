{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Frontend.LambdaQ.Par
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified Frontend.LambdaQ.Abs
import Frontend.LambdaQ.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap4 = HappyWrap4 (Double)
happyIn4 :: (Double) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap4 x)
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> HappyWrap4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 (Integer)
happyIn5 :: (Integer) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 (Frontend.LambdaQ.Abs.GateVar)
happyIn6 :: (Frontend.LambdaQ.Abs.GateVar) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 (Frontend.LambdaQ.Abs.Var)
happyIn7 :: (Frontend.LambdaQ.Abs.Var) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 (Frontend.LambdaQ.Abs.Lambda)
happyIn8 :: (Frontend.LambdaQ.Abs.Lambda) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 (Frontend.LambdaQ.Abs.Program)
happyIn9 :: (Frontend.LambdaQ.Abs.Program) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 (Frontend.LambdaQ.Abs.IntegerExpression)
happyIn10 :: (Frontend.LambdaQ.Abs.IntegerExpression) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 (Frontend.LambdaQ.Abs.IntegerExpression)
happyIn11 :: (Frontend.LambdaQ.Abs.IntegerExpression) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 (Frontend.LambdaQ.Abs.IntegerExpression)
happyIn12 :: (Frontend.LambdaQ.Abs.IntegerExpression) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 (Frontend.LambdaQ.Abs.IntegerExpression)
happyIn13 :: (Frontend.LambdaQ.Abs.IntegerExpression) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 (Frontend.LambdaQ.Abs.BoolValue)
happyIn14 :: (Frontend.LambdaQ.Abs.BoolValue) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 (Frontend.LambdaQ.Abs.BoolExpression)
happyIn15 :: (Frontend.LambdaQ.Abs.BoolExpression) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 (Frontend.LambdaQ.Abs.BoolExpression)
happyIn16 :: (Frontend.LambdaQ.Abs.BoolExpression) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 (Frontend.LambdaQ.Abs.BoolExpression)
happyIn17 :: (Frontend.LambdaQ.Abs.BoolExpression) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 (Frontend.LambdaQ.Abs.Type)
happyIn18 :: (Frontend.LambdaQ.Abs.Type) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 (Frontend.LambdaQ.Abs.Type)
happyIn19 :: (Frontend.LambdaQ.Abs.Type) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 (Frontend.LambdaQ.Abs.Type)
happyIn20 :: (Frontend.LambdaQ.Abs.Type) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 (Frontend.LambdaQ.Abs.Type)
happyIn21 :: (Frontend.LambdaQ.Abs.Type) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 (Frontend.LambdaQ.Abs.Type)
happyIn22 :: (Frontend.LambdaQ.Abs.Type) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 (Frontend.LambdaQ.Abs.Type)
happyIn23 :: (Frontend.LambdaQ.Abs.Type) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 (Frontend.LambdaQ.Abs.Angle)
happyIn24 :: (Frontend.LambdaQ.Abs.Angle) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 (Frontend.LambdaQ.Abs.BasisState)
happyIn25 :: (Frontend.LambdaQ.Abs.BasisState) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 (Frontend.LambdaQ.Abs.Gate)
happyIn26 :: (Frontend.LambdaQ.Abs.Gate) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 ([Frontend.LambdaQ.Abs.Var])
happyIn27 :: ([Frontend.LambdaQ.Abs.Var]) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 (Frontend.LambdaQ.Abs.ControlBasisState)
happyIn28 :: (Frontend.LambdaQ.Abs.ControlBasisState) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 (Frontend.LambdaQ.Abs.ControlBasisStates)
happyIn29 :: (Frontend.LambdaQ.Abs.ControlBasisStates) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 ([Frontend.LambdaQ.Abs.BasisState])
happyIn30 :: ([Frontend.LambdaQ.Abs.BasisState]) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 (Frontend.LambdaQ.Abs.ControlBit)
happyIn31 :: (Frontend.LambdaQ.Abs.ControlBit) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
newtype HappyWrap32 = HappyWrap32 (Frontend.LambdaQ.Abs.ControlBits)
happyIn32 :: (Frontend.LambdaQ.Abs.ControlBits) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap32 x)
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> HappyWrap32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 ([Integer])
happyIn33 :: ([Integer]) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
newtype HappyWrap34 = HappyWrap34 (Frontend.LambdaQ.Abs.ControlTerm)
happyIn34 :: (Frontend.LambdaQ.Abs.ControlTerm) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 (Frontend.LambdaQ.Abs.ControlTerms)
happyIn35 :: (Frontend.LambdaQ.Abs.ControlTerms) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 (Frontend.LambdaQ.Abs.ControlVar)
happyIn36 :: (Frontend.LambdaQ.Abs.ControlVar) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 (Frontend.LambdaQ.Abs.ControlVars)
happyIn37 :: (Frontend.LambdaQ.Abs.ControlVars) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 ([Frontend.LambdaQ.Abs.Term])
happyIn38 :: ([Frontend.LambdaQ.Abs.Term]) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 (Frontend.LambdaQ.Abs.Term)
happyIn39 :: (Frontend.LambdaQ.Abs.Term) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 (Frontend.LambdaQ.Abs.Term)
happyIn40 :: (Frontend.LambdaQ.Abs.Term) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 (Frontend.LambdaQ.Abs.Term)
happyIn41 :: (Frontend.LambdaQ.Abs.Term) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 (Frontend.LambdaQ.Abs.Term)
happyIn42 :: (Frontend.LambdaQ.Abs.Term) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 (Frontend.LambdaQ.Abs.Term)
happyIn43 :: (Frontend.LambdaQ.Abs.Term) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 (Frontend.LambdaQ.Abs.List)
happyIn44 :: (Frontend.LambdaQ.Abs.List) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 (Frontend.LambdaQ.Abs.List)
happyIn45 :: (Frontend.LambdaQ.Abs.List) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 (Frontend.LambdaQ.Abs.CaseExpression)
happyIn46 :: (Frontend.LambdaQ.Abs.CaseExpression) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 ([Frontend.LambdaQ.Abs.CaseExpression])
happyIn47 :: ([Frontend.LambdaQ.Abs.CaseExpression]) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 (Frontend.LambdaQ.Abs.Arg)
happyIn48 :: (Frontend.LambdaQ.Abs.Arg) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 ([Frontend.LambdaQ.Abs.Arg])
happyIn49 :: ([Frontend.LambdaQ.Abs.Arg]) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 (Frontend.LambdaQ.Abs.FunctionDefinition)
happyIn50 :: (Frontend.LambdaQ.Abs.FunctionDefinition) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 (Frontend.LambdaQ.Abs.FunctionType)
happyIn51 :: (Frontend.LambdaQ.Abs.FunctionType) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 (Frontend.LambdaQ.Abs.FunctionDeclaration)
happyIn52 :: (Frontend.LambdaQ.Abs.FunctionDeclaration) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 ([Frontend.LambdaQ.Abs.FunctionDeclaration])
happyIn53 :: ([Frontend.LambdaQ.Abs.FunctionDeclaration]) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\xc4\x00\x00\x00\x18\x24\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x30\x48\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\x00\x00\x00\x18\x24\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x03\x00\x00\x60\x90\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x62\x00\x00\x00\x0c\x12\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x31\x00\x00\x00\x06\x09\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x20\x00\xf0\x23\x00\x00\x00\x10\x58\x36\x45\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x40\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x80\x1f\x01\x00\x00\x80\xc0\xb2\x29\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x20\x00\xf0\x23\x00\x00\x00\x10\x58\x36\x45\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x08\x00\xfc\x08\x00\x00\x00\x04\x96\x4d\xd1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xba\xfd\xff\xff\xfd\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x06\x02\x00\x3f\x02\x00\x00\x00\x81\x65\x53\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x80\x00\x00\x00\x40\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x80\x81\x00\xc0\x8f\x00\x00\x00\x40\x60\xd9\x14\x0d\x00\x00\x00\x00\x00\x00\x00\x02\x46\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x10\x00\x00\x10\x00\x00\x00\x08\x00\x10\x20\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x40\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x80\x1f\x01\x00\x00\x80\xc0\xb2\x29\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x20\x00\xf0\x23\x00\x00\x00\x10\x18\x22\x40\x01\x00\x00\x00\x00\x00\x00\x30\x10\x00\xf8\x11\x00\x00\x00\x08\x0c\x11\xa0\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x02\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x80\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x80\x00\x00\x00\x40\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x31\x00\x00\x00\x06\x09\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x08\x00\xfc\x08\x00\x00\x00\x04\x96\x4d\xd1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x02\x00\x3f\x02\x00\x00\x00\x81\x21\x02\x14\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x02\x00\x3f\x02\x00\x00\x00\x81\x65\x53\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x20\x00\xf0\x23\x00\x00\x00\x10\x58\x36\x45\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x60\x20\x00\xf0\x23\x00\x00\x00\x10\x58\x36\x45\x03\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x0f\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf8\x01\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x02\x00\x3f\x02\x00\x00\x00\x81\x65\x53\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x20\x00\xf0\x23\x00\x00\x00\x10\x58\x36\x45\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x80\x1f\x01\x00\x00\x80\xc0\xb2\x29\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x08\x00\xfc\x08\x00\x00\x00\x04\x96\x4d\xd1\x00\x00\x00\x00\x00\x00\x00\x0c\x04\x00\x7e\x04\x00\x00\x00\x02\xcb\xa6\x68\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x40\x00\xe0\x47\x00\x00\x00\x20\xb0\x6c\x8a\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x08\x04\xfc\x08\x00\x00\x00\x04\x96\x4d\xd1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\xc0\x40\x00\xe0\x47\x00\x00\x00\x20\xb0\x6c\x8a\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x04\x00\x7e\x04\x00\x00\x00\x02\xcb\xa6\x68\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x04\x00\x7e\x04\x00\x00\x00\x02\xcb\xa6\x68\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x18\x08\x00\xfc\x08\x00\x00\x00\x04\x96\x4d\xd1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\xf8\x11\x00\x00\x00\x08\x2c\x9b\xa2\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x80\x81\x00\xc0\x8f\x00\x00\x00\x40\x60\xd9\x14\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\xf8\x11\x00\x00\x00\x08\x2c\x9b\xa2\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","Double","Integer","GateVar","Var","Lambda","Program","IntegerExpression1","IntegerExpression2","IntegerExpression3","IntegerExpression","BoolValue","BoolExpression","BoolExpression1","BoolExpression2","Type","Type2","Type3","Type5","Type1","Type4","Angle","BasisState","Gate","ListVar","ControlBasisState","ControlBasisStates","ListBasisState","ControlBit","ControlBits","ListInteger","ControlTerm","ControlTerms","ControlVar","ControlVars","ListTerm","Term4","Term3","Term2","Term1","Term","List1","List","CaseExpression","ListCaseExpression","Arg","ListArg","FunctionDefinition","FunctionType","FunctionDeclaration","ListFunctionDeclaration","'!'","'!!'","'$'","'&&'","'('","'()'","')'","'*'","'**'","'+'","'++'","','","'-'","'->'","'.'","'/'","'/='","':'","'::'","';'","'<'","'<-'","'<='","'='","'=='","'>'","'>='","'@+'","'@+i'","'@-'","'@-i'","'@0'","'@1'","'Bit'","'Bool'","'FSWAP'","'False'","'H'","'ID'","'ISWAP'","'Int'","'QFT'","'QFT_DAG'","'Qbit'","'ROOT_SWAP'","'ROOT_SWAP_DAG'","'ROOT_X'","'ROOT_X_DAG'","'ROOT_Y'","'ROOT_Y_DAG'","'ROOT_Z'","'ROOT_Z_DAG'","'RX'","'RY'","'RZ'","'S'","'SQRT_SWAP'","'SQRT_SWAP_DAG'","'SQRT_X'","'SQRT_X_DAG'","'SQRT_Y'","'SQRT_Y_DAG'","'SWAP'","'SWAP_THETA'","'S_DAG'","'T'","'T_DAG'","'True'","'U1'","'U2'","'U3'","'X'","'Y'","'Z'","'['","'[]'","']'","'case'","'ctrl'","'else'","'gate'","'if'","'in'","'let'","'not'","'of'","'then'","'with'","'{'","'|'","'||'","'}'","L_doubl","L_integ","L_GateVar","L_Var","L_Lambda","%eof"]
        bit_start = st Prelude.* 151
        bit_end = (st Prelude.+ 1) Prelude.* 151
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..150]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xae\xff\xc1\xff\x00\x00\x11\x00\xcb\xff\x2e\x00\xef\xff\x00\x00\x00\x00\x00\x00\xef\xff\xaa\x00\x47\x00\x5f\x00\x00\x00\x00\x00\x00\x00\x98\x00\x8f\x00\xaa\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaa\x00\x4e\x00\xab\x00\xdc\xff\x00\x00\x79\x00\x01\x01\xf8\xff\x7a\x00\x00\x00\xc6\x00\xaa\x00\xaa\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x00\x00\x0f\x01\xcd\x00\x37\x01\x1b\x01\x00\x00\x00\x00\x00\x00\x12\x00\xd1\x00\x00\x00\x00\x00\x00\x00\x1d\x01\x71\x00\x2e\x01\x00\x00\x00\x00\x2a\x00\x1a\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x1a\x00\xc9\x00\x1a\x00\xda\x00\x05\x00\x04\x01\xf1\x00\x00\x00\x70\x01\x2f\x01\x35\x01\x3c\x01\x1a\x00\x1b\x01\xd1\x00\x1e\x00\xff\xff\x26\x01\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x01\x48\x01\x48\x01\x48\x01\x48\x01\x48\x01\x48\x01\x48\x01\x48\x01\x48\x01\x4a\x01\x4a\x01\x4a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x01\x00\x00\x00\x00\x00\x00\x4a\x01\xa0\x01\xa8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x01\xf7\xff\xde\x00\x00\x00\x06\x00\xc3\x01\x22\x00\x46\x01\xa2\x00\x6d\x01\x07\x00\x1a\x00\x00\x00\xbb\x01\x7d\x00\x7d\x00\x07\x00\x05\x00\x05\x00\x05\x00\x05\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\x03\x00\xaa\x00\x1a\x00\xba\x01\x99\x00\xde\x00\xde\x00\xde\x00\xde\x00\xde\x00\xde\x00\xde\x00\xde\x00\x00\x00\x00\x00\x15\x01\x15\x01\x00\x00\x00\x00\x00\x00\x7d\x00\xbd\x01\xbd\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x00\x00\xc9\x01\x1a\x00\x00\x00\x80\x01\x8a\x01\x8a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8a\x01\x1a\x00\xd2\x01\x94\x01\x22\x00\x01\x00\xf9\xff\xab\x01\xab\x01\xac\x01\x98\x01\xe7\x01\xb6\x01\x00\x00\x00\x00\x13\x00\x00\x00\x00\x00\x13\x00\x00\x00\x00\x00\x1a\x00\x00\x00\xb3\x01\xfa\x01\x1a\x00\xd8\x01\x16\x02\x19\x02\x2e\x02\x1a\x00\xf0\x01\x43\x02\x4b\x02\x52\x02\x1a\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x59\x02\xfc\xff\x1d\x02\x1f\x02\x1f\x02\x1f\x02\x1a\x00\x32\x02\x31\x02\x56\x02\x6b\x02\x9a\x02\xaf\x02\x80\x02\x83\x02\xcc\x02\x85\x02\x00\x00\x1a\x00\x00\x00\x00\x00\x00\x02\x99\x02\x00\x00\x00\x00\xf3\x02\xb9\x02\x00\x00\x47\x01\x08\x03\x15\x03\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x00\x00\xc7\x02\x00\x00\x00\x00\xc7\x02\x1a\x00\x1e\x03\x2d\x03\xfe\x02\x42\x03\x13\x03\x4f\x03\x1a\x00\x00\x00\x00\x02\x00\x00\x17\x03\x1a\x00\x00\x00\x71\x03\x83\x03\x00\x00\x00\x00\x31\x03\x00\x00\x00\x00\x00\x00\x4c\x03\x1a\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x94\x00\x00\x00\x00\x00\x00\x00\x0f\x00\xfd\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd2\x00\x03\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x04\x1f\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x03\x68\x01\x4e\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x02\x00\x00\x00\x00\x00\x00\x9f\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9d\x03\x00\x00\x00\x00\x00\x00\x00\x00\x23\x02\x00\x00\x81\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x38\x02\x00\x00\x4d\x02\x73\x00\x62\x02\x00\x00\x79\x01\x83\x01\xb1\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x77\x02\x00\x00\x00\x00\x4f\x01\xb3\x03\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb6\x03\xc1\x03\xc2\x03\xc3\x03\xc8\x03\xca\x03\xcb\x03\xd6\x03\xd7\x03\xd8\x03\x7f\x00\x84\x00\x8b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\xa3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaa\x01\x00\x00\x00\x00\x00\x00\x00\x00\xdd\x03\xb8\x03\x8c\x02\x00\x00\x00\x00\xb2\x03\xc7\x03\xb9\x03\xf4\x01\xf8\x01\xa2\x01\xcb\x01\xdf\x00\x0c\x01\xd3\x01\xd5\x01\xfc\x01\xeb\x03\xed\x03\xf5\x03\xfa\x03\x02\x04\x0f\x04\xa1\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdc\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x01\x00\x00\x00\x00\x00\x00\x93\x01\x00\x00\x00\x00\xa5\x00\xa6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xac\x00\xb6\x02\x00\x00\xea\x03\x00\x00\x00\x00\x00\x00\x53\x01\x7c\x01\xa6\x01\xa7\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x00\x00\xb5\x00\x00\x00\x00\x00\xbc\x01\x00\x00\xc8\x00\x00\x00\xcb\x02\x00\x00\x00\x00\x00\x00\x00\x00\x14\x01\x00\x00\x00\x00\x00\x00\x00\x00\xe0\x02\xf5\x02\x00\x00\x00\x00\x00\x00\xe5\x01\x00\x00\x00\x00\x3f\x01\x00\x00\xd0\x00\xd4\x00\xd5\x00\x0a\x03\x00\x00\xdb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x01\x00\x00\x1f\x03\x00\x00\x00\x00\xc7\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x03\x00\x00\x00\x00\xd7\x00\x00\x00\x00\x00\xd8\x00\x49\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x03\x00\x00\x0d\x01\x00\x00\x6f\x00\x73\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x03\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x5c\xff\x00\x00\xfe\xff\x00\x00\x00\x00\x00\x00\x5c\xff\xf9\xff\xfb\xff\x5b\xff\x5e\xff\x00\x00\x5f\xff\xcf\xff\xda\xff\xce\xff\xdc\xff\xd7\xff\x00\x00\x00\x00\xd2\xff\xd5\xff\xd6\xff\xd4\xff\xd3\xff\x00\x00\x63\xff\x00\x00\x60\xff\x64\xff\x63\xff\x00\x00\x00\x00\x00\x00\xd8\xff\x00\x00\x00\x00\x00\x00\xdd\xff\xdb\xff\xd9\xff\xfd\xff\xd0\xff\xd1\xff\x00\x00\x62\xff\xf1\xff\x86\xff\x00\x00\xef\xff\xf5\xff\xf2\xff\x89\xff\xdf\xff\x8a\xff\xe9\xff\xe6\xff\x8b\xff\x84\xff\x7a\xff\x72\xff\x70\xff\x61\xff\x69\xff\x87\xff\x00\x00\x8c\xff\x00\x00\xca\xff\xc8\xff\xc9\xff\xc7\xff\xcc\xff\xcb\xff\xed\xff\xee\xff\x00\x00\x6f\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfa\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xea\xff\x00\x00\x00\x00\x00\x00\x9f\xff\x88\xff\xa9\xff\xc6\xff\xc2\xff\xaa\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbb\xff\xac\xff\xab\xff\xb7\xff\xb6\xff\xb5\xff\xb4\xff\xad\xff\x00\x00\xba\xff\xb9\xff\xb8\xff\x00\x00\x00\x00\x00\x00\xc5\xff\xc4\xff\xc3\xff\xfc\xff\x00\x00\x00\x00\xf5\xff\xf8\xff\x00\x00\x89\xff\x8a\xff\x00\x00\x87\xff\x00\x00\x00\x00\x00\x00\x86\xff\x7d\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\xff\xf7\xff\xe2\xff\xe3\xff\xe5\xff\xe0\xff\xe1\xff\xe4\xff\xf3\xff\xf4\xff\xeb\xff\xec\xff\xe8\xff\xe7\xff\x6a\xff\x00\x00\x7c\xff\x7b\xff\x71\xff\x6b\xff\x8e\xff\x6c\xff\x8d\xff\x00\x00\xde\xff\xf0\xff\x00\x00\x00\x00\x6e\xff\x00\x00\x00\x00\x00\x00\xcd\xff\xb0\xff\xa8\xff\xb1\xff\xb2\xff\xb3\xff\xbc\xff\xbd\xff\xbe\xff\xbf\xff\xc0\xff\xc1\xff\xa6\xff\xa7\xff\xa4\xff\xa5\xff\xa0\xff\xa1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x86\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9e\xff\x00\x00\x83\xff\x80\xff\x00\x00\x82\xff\x7f\xff\x00\x00\x81\xff\x7e\xff\x00\x00\x94\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\xff\x00\x00\x00\x00\x00\x00\x00\x00\x76\xff\x73\xff\x85\xff\x00\x00\x6d\xff\x00\x00\x66\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9d\xff\x00\x00\x9c\xff\x98\xff\x00\x00\x00\x00\x93\xff\x91\xff\x00\x00\x00\x00\x79\xff\x00\x00\x00\x00\x00\x00\x74\xff\x65\xff\x67\xff\x00\x00\x8f\xff\x68\xff\x00\x00\xaf\xff\xa2\xff\x00\x00\x00\x00\x00\x00\x96\xff\x00\x00\x9a\xff\x00\x00\x00\x00\x00\x00\x9b\xff\x00\x00\x97\xff\x00\x00\x00\x00\x78\xff\x00\x00\x00\x00\xae\xff\xa3\xff\x00\x00\x95\xff\x99\xff\x75\xff\x00\x00\x00\x00\x77\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x05\x00\x06\x00\x0c\x00\x05\x00\x0c\x00\x0e\x00\x05\x00\x05\x00\x0d\x00\x05\x00\x05\x00\x05\x00\x0c\x00\x60\x00\x0d\x00\x14\x00\x05\x00\x03\x00\x0d\x00\x00\x00\x01\x00\x04\x00\x16\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x5d\x00\x05\x00\x06\x00\x25\x00\x03\x00\x05\x00\x13\x00\x01\x00\x04\x00\x0d\x00\x14\x00\x07\x00\x25\x00\x0d\x00\x02\x00\x62\x00\x03\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x0b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x60\x00\x2e\x00\x62\x00\x25\x00\x44\x00\x1d\x00\x14\x00\x25\x00\x4d\x00\x4d\x00\x4d\x00\x4b\x00\x4c\x00\x44\x00\x4e\x00\x2c\x00\x2d\x00\x51\x00\x52\x00\x60\x00\x54\x00\x55\x00\x4b\x00\x4c\x00\x58\x00\x0e\x00\x5a\x00\x2c\x00\x2d\x00\x03\x00\x5e\x00\x05\x00\x60\x00\x61\x00\x44\x00\x60\x00\x5e\x00\x5e\x00\x44\x00\x5e\x00\x5e\x00\x4b\x00\x4c\x00\x08\x00\x4e\x00\x5d\x00\x5e\x00\x51\x00\x52\x00\x5b\x00\x54\x00\x55\x00\x01\x00\x5e\x00\x58\x00\x55\x00\x5a\x00\x02\x00\x05\x00\x06\x00\x5e\x00\x08\x00\x60\x00\x61\x00\x5e\x00\x5b\x00\x0d\x00\x00\x00\x0f\x00\x07\x00\x05\x00\x06\x00\x00\x00\x2f\x00\x30\x00\x31\x00\x0e\x00\x16\x00\x0d\x00\x00\x00\x1d\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x14\x00\x05\x00\x06\x00\x25\x00\x03\x00\x14\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x14\x00\x00\x00\x09\x00\x25\x00\x00\x00\x02\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x07\x00\x03\x00\x01\x00\x00\x00\x0b\x00\x60\x00\x05\x00\x06\x00\x22\x00\x23\x00\x01\x00\x14\x00\x44\x00\x01\x00\x14\x00\x29\x00\x14\x00\x14\x00\x2c\x00\x4b\x00\x4c\x00\x17\x00\x14\x00\x14\x00\x44\x00\x51\x00\x2f\x00\x30\x00\x31\x00\x55\x00\x15\x00\x4b\x00\x4c\x00\x15\x00\x03\x00\x22\x00\x23\x00\x51\x00\x5e\x00\x00\x00\x60\x00\x55\x00\x29\x00\x00\x00\x00\x00\x2c\x00\x00\x00\x00\x00\x60\x00\x4b\x00\x5e\x00\x15\x00\x60\x00\x03\x00\x17\x00\x01\x00\x1a\x00\x11\x00\x11\x00\x14\x00\x13\x00\x08\x00\x08\x00\x14\x00\x14\x00\x19\x00\x14\x00\x14\x00\x24\x00\x10\x00\x26\x00\x27\x00\x28\x00\x17\x00\x2a\x00\x2b\x00\x4b\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x01\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x08\x00\x01\x00\x03\x00\x03\x00\x04\x00\x18\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x15\x00\x08\x00\x5e\x00\x16\x00\x11\x00\x1a\x00\x5f\x00\x15\x00\x17\x00\x10\x00\x11\x00\x60\x00\x19\x00\x12\x00\x15\x00\x03\x00\x17\x00\x59\x00\x19\x00\x1a\x00\x1b\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x01\x00\x0a\x00\x03\x00\x04\x00\x0d\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x07\x00\x07\x00\x4b\x00\x01\x00\x60\x00\x0c\x00\x0c\x00\x15\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x01\x00\x19\x00\x03\x00\x04\x00\x1c\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x10\x00\x11\x00\x01\x00\x13\x00\x0c\x00\x57\x00\x4f\x00\x15\x00\x07\x00\x08\x00\x01\x00\x0a\x00\x4f\x00\x0c\x00\x0d\x00\x06\x00\x07\x00\x08\x00\x09\x00\x4f\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x01\x00\x19\x00\x03\x00\x04\x00\x1c\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1e\x00\x1f\x00\x01\x00\x21\x00\x05\x00\x5e\x00\x5d\x00\x15\x00\x07\x00\x08\x00\x01\x00\x0a\x00\x05\x00\x0c\x00\x0d\x00\x06\x00\x07\x00\x08\x00\x09\x00\x56\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x01\x00\x18\x00\x03\x00\x04\x00\x1b\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x07\x00\x5e\x00\x01\x00\x12\x00\x14\x00\x12\x00\x07\x00\x15\x00\x07\x00\x08\x00\x01\x00\x0a\x00\x01\x00\x0c\x00\x0d\x00\x59\x00\x07\x00\x08\x00\x07\x00\x08\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x01\x00\x5d\x00\x03\x00\x04\x00\x18\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0c\x00\x60\x00\x01\x00\x4b\x00\x4b\x00\x60\x00\x01\x00\x15\x00\x07\x00\x08\x00\x01\x00\x0a\x00\x07\x00\x08\x00\x0d\x00\x0a\x00\x07\x00\x08\x00\x0d\x00\x0c\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x01\x00\x5a\x00\x03\x00\x04\x00\x60\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x0c\x00\x15\x00\x01\x00\x0c\x00\x03\x00\x04\x00\x50\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x0c\x00\x03\x00\x04\x00\x4d\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x0c\x00\x03\x00\x04\x00\x07\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x12\x00\x03\x00\x04\x00\x0e\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x5c\x00\x03\x00\x04\x00\x5d\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x5c\x00\x03\x00\x04\x00\x60\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x4d\x00\x03\x00\x04\x00\x0c\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x4d\x00\x03\x00\x04\x00\x0c\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x4d\x00\x03\x00\x04\x00\x4d\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x16\x00\x03\x00\x04\x00\x60\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x5e\x00\x03\x00\x04\x00\x07\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x53\x00\x03\x00\x04\x00\x07\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x0c\x00\x03\x00\x04\x00\x5d\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x18\x00\x03\x00\x04\x00\x0c\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x4d\x00\x03\x00\x04\x00\x0c\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x4d\x00\x03\x00\x04\x00\x14\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x5e\x00\x03\x00\x04\x00\x07\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x07\x00\x03\x00\x04\x00\x5c\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x53\x00\x03\x00\x01\x00\x03\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x15\x00\x01\x00\x03\x00\x03\x00\x03\x00\x01\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x01\x00\x01\x00\x01\x00\x28\x00\x29\x00\x15\x00\x01\x00\x01\x00\x03\x00\x01\x00\x01\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x01\x00\x01\x00\x01\x00\x28\x00\x29\x00\x15\x00\x01\x00\x01\x00\x03\x00\x28\x00\x28\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x23\x00\x24\x00\x01\x00\x03\x00\x01\x00\x28\x00\x29\x00\x15\x00\x07\x00\x08\x00\x07\x00\x08\x00\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\x01\x00\x07\x00\x08\x00\xff\xff\x23\x00\x24\x00\x07\x00\x08\x00\x01\x00\x28\x00\x29\x00\xff\xff\xff\xff\xff\xff\x07\x00\x08\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x42\x00\x43\x00\xc5\x00\xdf\x00\xf1\x00\x26\x00\x8b\x00\x8b\x00\x44\x00\x5e\x00\x8b\x00\xb9\x00\xf3\x00\x09\x00\x44\x00\x27\x01\xdc\x00\x1a\x00\x44\x00\xc9\x00\xd9\x00\x9a\x00\xa7\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x03\x00\x42\x00\x43\x00\x4b\x00\x1d\x00\x5e\x00\x0c\x00\x30\x01\x9a\x00\x44\x00\xda\x00\xc2\x00\x4b\x00\x44\x00\x90\x00\xff\xff\x1d\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x91\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x5d\xff\x1b\x00\x5d\xff\x4b\x00\x4c\x00\x31\x01\x0b\x00\x4b\x00\xc6\x00\x2c\x00\xf2\x00\x4d\x00\x4e\x00\x4c\x00\x4f\x00\x1e\x00\x1f\x00\x50\x00\x51\x00\x09\x00\x52\x00\x53\x00\x4d\x00\x4e\x00\x54\x00\x26\x00\x55\x00\x1e\x00\x2d\x00\x03\x00\x2a\x00\x04\x00\x09\x00\x56\x00\x4c\x00\x09\x00\x2a\x00\x2a\x00\x4c\x00\x2a\x00\x2a\x00\x4d\x00\x4e\x00\x25\x00\x4f\x00\x03\x00\x2a\x00\x50\x00\x51\x00\x9b\x00\x52\x00\x53\x00\x30\x01\x2a\x00\x54\x00\x53\x00\x55\x00\x60\x00\x42\x00\x43\x00\x2a\x00\x95\x00\x09\x00\x56\x00\x2a\x00\x9b\x00\x44\x00\xc9\x00\x96\x00\x2b\x00\x42\x00\x43\x00\xc9\x00\x05\x00\x06\x00\x07\x00\x26\x00\x61\x00\x44\x00\xc9\x00\x41\x01\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xce\x00\x14\x00\x15\x00\x4b\x00\x03\x00\xcd\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xcc\x00\xc9\x00\x24\x00\x4b\x00\xc9\x00\x90\x00\xc9\x00\xc9\x00\x26\x00\xff\x00\xbf\x00\xe6\x00\x13\x00\xc9\x00\x91\x00\x09\x00\x14\x00\x15\x00\x16\x00\x17\x00\x12\x01\xcb\x00\x4c\x00\x10\x01\xca\x00\x18\x00\xf8\x00\xf7\x00\x19\x00\x4d\x00\x4e\x00\xe7\x00\x1d\x00\xf6\x00\x4c\x00\x50\x00\x05\x00\x06\x00\x09\x00\x53\x00\x13\x01\x4d\x00\x4e\x00\x11\x01\xe6\x00\x16\x00\x17\x00\x50\x00\x2a\x00\xc9\x00\x09\x00\x53\x00\x18\x00\xc9\x00\xc9\x00\x19\x00\xc9\x00\xc9\x00\x09\x00\x1a\x00\x2a\x00\x32\x01\x09\x00\xe6\x00\x0e\x01\x2e\x00\x33\x01\x98\x00\x0f\x00\x23\x01\x22\x00\x9c\x00\xb2\x00\x22\x01\x21\x01\x99\x00\x3d\x01\x3c\x01\x63\x00\x9d\x00\x64\x00\x65\x00\x66\x00\x1e\x01\x67\x00\x68\x00\x1a\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x7b\x00\x7c\x00\x7d\x00\x7e\x00\x7f\x00\x2e\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x85\x00\xb1\x00\x2e\x00\xe6\x00\x2f\x00\x30\x00\x2d\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x32\x01\x9c\x00\x2a\x00\xa7\x00\x98\x00\x42\x01\x86\x00\x39\x00\x16\x01\x9d\x00\x9e\x00\x09\x00\x99\x00\x97\x00\x9f\x00\x92\x00\xa0\x00\x5f\x00\xa1\x00\xa2\x00\xa3\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x05\x01\x3f\x00\x40\x00\x06\x01\x07\x01\x2e\x00\xa4\x00\x2f\x00\x30\x00\xa5\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\xc0\x00\x2d\x01\x5b\x00\x2e\x00\x09\x00\xc1\x00\x2e\x01\x39\x00\x31\x00\x32\x00\x33\x00\xc3\x00\x35\x00\xdf\x00\x37\x00\x38\x00\x0d\x00\x0e\x00\x0f\x00\x26\x00\x11\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x05\x01\x3f\x00\x40\x00\x06\x01\x25\x01\x2e\x00\xee\x00\x2f\x00\x30\x00\xef\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x27\x00\x0f\x00\x2e\x00\x11\x00\xe6\x00\xdd\x00\xe5\x00\x39\x00\x5b\x00\x33\x00\x2e\x00\x35\x00\xe4\x00\x5c\x00\x38\x00\x31\x00\x88\x00\x33\x00\x89\x00\xe3\x00\xfc\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\xfb\x00\x3f\x00\x40\x00\x2e\x00\xeb\x00\x2f\x00\x30\x00\xec\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x57\x00\x58\x00\x2e\x00\x59\x00\xc9\x00\x2a\x00\x03\x00\x39\x00\x5b\x00\x33\x00\x2e\x00\x35\x00\xc8\x00\xb4\x00\x38\x00\x31\x00\x88\x00\x33\x00\xc3\x00\xc7\x00\xfa\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\xfb\x00\x3f\x00\x40\x00\x2e\x00\xe8\x00\x2f\x00\x30\x00\xe9\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\xc3\x00\x2a\x00\x2e\x00\x97\x00\x00\x01\x97\x00\xc3\x00\x39\x00\x5b\x00\x33\x00\x2e\x00\x35\x00\x2e\x00\xb3\x00\x38\x00\xfa\x00\xb0\x00\x33\x00\xaf\x00\x33\x00\x0f\x01\x3a\x00\x3b\x00\x3c\x00\x3d\x00\xfb\x00\x3f\x00\x40\x00\x2e\x00\x03\x00\x2f\x00\x30\x00\xf5\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x16\x01\x09\x00\x2e\x00\xee\x00\xeb\x00\x09\x00\x2e\x00\x39\x00\x5b\x00\x33\x00\x2e\x00\x35\x00\x5b\x00\x33\x00\xb6\x00\x35\x00\xae\x00\x33\x00\xb5\x00\x0e\x01\x28\x01\x3a\x00\x3b\x00\x3c\x00\x3d\x00\xfb\x00\x3f\x00\x40\x00\x2e\x00\x15\x01\x2f\x00\x30\x00\x09\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x0b\x01\x39\x00\x2e\x00\x0a\x01\x2f\x00\x30\x00\x0c\x01\x31\x00\x32\x00\x33\x00\x8b\x00\x35\x00\x8c\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x39\x00\x2e\x00\x09\x01\x2f\x00\x30\x00\x05\x01\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x8d\x00\x3f\x00\x8e\x00\x39\x00\x2e\x00\x04\x01\x2f\x00\x30\x00\x03\x01\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x87\x00\x3f\x00\x40\x00\x39\x00\x2e\x00\x97\x00\x2f\x00\x30\x00\x28\x01\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x86\x00\x3f\x00\x40\x00\x39\x00\x2e\x00\x25\x01\xe0\x00\x30\x00\x03\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x5f\x00\x3f\x00\x40\x00\x39\x00\x2e\x00\x20\x01\x2f\x00\x30\x00\x09\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\xe1\x00\x3f\x00\x40\x00\x39\x00\x2e\x00\x1e\x01\x2f\x00\x30\x00\x1c\x01\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\xbb\x00\x3f\x00\x40\x00\x39\x00\x2e\x00\x1d\x01\x2f\x00\x30\x00\x1b\x01\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\xa7\x00\x3f\x00\x40\x00\x39\x00\x2e\x00\x1a\x01\x2f\x00\x30\x00\x19\x01\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\xf5\x00\x3f\x00\x40\x00\x39\x00\x2e\x00\x18\x01\x2f\x00\x30\x00\x09\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x0c\x01\x3f\x00\x40\x00\x39\x00\x2e\x00\x2a\x00\x2f\x00\x30\x00\x30\x01\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x01\x01\x3f\x00\x40\x00\x39\x00\x2e\x00\x2f\x01\x2f\x00\x30\x00\x2c\x01\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x00\x01\x3f\x00\x40\x00\x39\x00\x2e\x00\x2b\x01\x2f\x00\x30\x00\x03\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x20\x01\x3f\x00\x40\x00\x39\x00\x2e\x00\x3b\x01\x2f\x00\x30\x00\x3a\x01\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x34\x01\x3f\x00\x40\x00\x39\x00\x2e\x00\x39\x01\x2f\x00\x30\x00\x38\x01\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x29\x01\x3f\x00\x40\x00\x39\x00\x2e\x00\x37\x01\x2f\x00\x30\x00\x36\x01\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3b\x01\x3f\x00\x40\x00\x39\x00\x2e\x00\x2a\x00\x2f\x00\x30\x00\x40\x01\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x43\x01\x3f\x00\x40\x00\x39\x00\x2e\x00\x3f\x01\x2f\x00\x30\x00\x45\x01\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x40\x01\x3f\x00\x40\x00\x39\x00\x2e\x00\x46\x01\x92\x00\x28\x00\xa5\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x46\x01\x3f\x00\x40\x00\x39\x00\x2e\x00\x56\x00\x92\x00\xdd\x00\xd8\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\x93\x00\xd7\x00\xd6\x00\xd5\x00\x3f\x00\x40\x00\x39\x00\x2e\x00\xd4\x00\x92\x00\xd3\x00\xd2\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\xba\x00\xd1\x00\xd0\x00\xcf\x00\x3f\x00\x40\x00\x39\x00\x2e\x00\xbd\x00\x92\x00\xbc\x00\xb7\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3a\x00\xb9\x00\x2e\x00\xf3\x00\x2e\x00\x3f\x00\x40\x00\x39\x00\xad\x00\x33\x00\xac\x00\x33\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\xab\x00\x33\x00\x00\x00\x3a\x00\xfd\x00\xaa\x00\x33\x00\x2e\x00\x3f\x00\x8e\x00\x00\x00\x00\x00\x00\x00\xa9\x00\x33\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x21\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x20\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\xa8\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 164) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136),
	(137 , happyReduce_137),
	(138 , happyReduce_138),
	(139 , happyReduce_139),
	(140 , happyReduce_140),
	(141 , happyReduce_141),
	(142 , happyReduce_142),
	(143 , happyReduce_143),
	(144 , happyReduce_144),
	(145 , happyReduce_145),
	(146 , happyReduce_146),
	(147 , happyReduce_147),
	(148 , happyReduce_148),
	(149 , happyReduce_149),
	(150 , happyReduce_150),
	(151 , happyReduce_151),
	(152 , happyReduce_152),
	(153 , happyReduce_153),
	(154 , happyReduce_154),
	(155 , happyReduce_155),
	(156 , happyReduce_156),
	(157 , happyReduce_157),
	(158 , happyReduce_158),
	(159 , happyReduce_159),
	(160 , happyReduce_160),
	(161 , happyReduce_161),
	(162 , happyReduce_162),
	(163 , happyReduce_163),
	(164 , happyReduce_164)
	]

happy_n_terms = 99 :: Prelude.Int
happy_n_nonterms = 50 :: Prelude.Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TD happy_var_1)) -> 
	happyIn4
		 ((read happy_var_1) :: Double
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn5
		 ((read happy_var_1) :: Integer
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (Frontend.LambdaQ.Abs.GateVar (mkPosToken happy_var_1)
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn7
		 (Frontend.LambdaQ.Abs.Var (mkPosToken happy_var_1)
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_Lambda happy_var_1)) -> 
	happyIn8
		 (Frontend.LambdaQ.Abs.Lambda happy_var_1
	)}

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	happyIn9
		 (Frontend.LambdaQ.Abs.ProgDef happy_var_1
	)}

happyReduce_7 = happySpecReduce_2  6# happyReduction_7
happyReduction_7 happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_2 of { (HappyWrap13 happy_var_2) -> 
	happyIn10
		 (Frontend.LambdaQ.Abs.ArithmExprMinus happy_var_2
	)}

happyReduce_8 = happySpecReduce_3  6# happyReduction_8
happyReduction_8 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	happyIn10
		 (Frontend.LambdaQ.Abs.ArithmExprAdd happy_var_1 happy_var_3
	)}}

happyReduce_9 = happySpecReduce_3  6# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	happyIn10
		 (Frontend.LambdaQ.Abs.ArithmExprSub happy_var_1 happy_var_3
	)}}

happyReduce_10 = happySpecReduce_1  6# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	happyIn10
		 (happy_var_1
	)}

happyReduce_11 = happySpecReduce_3  7# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn11
		 (Frontend.LambdaQ.Abs.ArithmExprMul happy_var_1 happy_var_3
	)}}

happyReduce_12 = happySpecReduce_3  7# happyReduction_12
happyReduction_12 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn11
		 (Frontend.LambdaQ.Abs.ArithmExprDiv happy_var_1 happy_var_3
	)}}

happyReduce_13 = happySpecReduce_1  7# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	happyIn11
		 (happy_var_1
	)}

happyReduce_14 = happySpecReduce_1  8# happyReduction_14
happyReduction_14 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn12
		 (Frontend.LambdaQ.Abs.ArithmExprInt happy_var_1
	)}

happyReduce_15 = happySpecReduce_3  8# happyReduction_15
happyReduction_15 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_2 of { (HappyWrap13 happy_var_2) -> 
	happyIn12
		 (happy_var_2
	)}

happyReduce_16 = happySpecReduce_1  9# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	happyIn13
		 (happy_var_1
	)}

happyReduce_17 = happySpecReduce_1  10# happyReduction_17
happyReduction_17 happy_x_1
	 =  happyIn14
		 (Frontend.LambdaQ.Abs.BoolValueTrue
	)

happyReduce_18 = happySpecReduce_1  10# happyReduction_18
happyReduction_18 happy_x_1
	 =  happyIn14
		 (Frontend.LambdaQ.Abs.BoolValueFalse
	)

happyReduce_19 = happySpecReduce_3  11# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	case happyOut16 happy_x_3 of { (HappyWrap16 happy_var_3) -> 
	happyIn15
		 (Frontend.LambdaQ.Abs.BoolExpressionAnd happy_var_1 happy_var_3
	)}}

happyReduce_20 = happySpecReduce_3  11# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	case happyOut16 happy_x_3 of { (HappyWrap16 happy_var_3) -> 
	happyIn15
		 (Frontend.LambdaQ.Abs.BoolExpressionOr happy_var_1 happy_var_3
	)}}

happyReduce_21 = happySpecReduce_2  11# happyReduction_21
happyReduction_21 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { (HappyWrap16 happy_var_2) -> 
	happyIn15
		 (Frontend.LambdaQ.Abs.BoolExpressionNot happy_var_2
	)}

happyReduce_22 = happySpecReduce_1  11# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	happyIn15
		 (happy_var_1
	)}

happyReduce_23 = happySpecReduce_3  12# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn16
		 (Frontend.LambdaQ.Abs.BoolExpressionEq happy_var_1 happy_var_3
	)}}

happyReduce_24 = happySpecReduce_3  12# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn16
		 (Frontend.LambdaQ.Abs.BoolExpressionDif happy_var_1 happy_var_3
	)}}

happyReduce_25 = happySpecReduce_1  12# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	happyIn16
		 (happy_var_1
	)}

happyReduce_26 = happySpecReduce_3  13# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	happyIn17
		 (Frontend.LambdaQ.Abs.BoolExpressionEqInt happy_var_1 happy_var_3
	)}}

happyReduce_27 = happySpecReduce_3  13# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	happyIn17
		 (Frontend.LambdaQ.Abs.BoolExpressionDifInt happy_var_1 happy_var_3
	)}}

happyReduce_28 = happySpecReduce_3  13# happyReduction_28
happyReduction_28 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	happyIn17
		 (Frontend.LambdaQ.Abs.BoolExpressionGt happy_var_1 happy_var_3
	)}}

happyReduce_29 = happySpecReduce_3  13# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	happyIn17
		 (Frontend.LambdaQ.Abs.BoolExpressionGe happy_var_1 happy_var_3
	)}}

happyReduce_30 = happySpecReduce_3  13# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	happyIn17
		 (Frontend.LambdaQ.Abs.BoolExpressionLt happy_var_1 happy_var_3
	)}}

happyReduce_31 = happySpecReduce_3  13# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	case happyOut11 happy_x_3 of { (HappyWrap11 happy_var_3) -> 
	happyIn17
		 (Frontend.LambdaQ.Abs.BoolExpressionLe happy_var_1 happy_var_3
	)}}

happyReduce_32 = happySpecReduce_1  13# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
	happyIn17
		 (Frontend.LambdaQ.Abs.BoolExpressionVal happy_var_1
	)}

happyReduce_33 = happySpecReduce_3  13# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_2 of { (HappyWrap15 happy_var_2) -> 
	happyIn17
		 (happy_var_2
	)}

happyReduce_34 = happySpecReduce_3  14# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut22 happy_x_3 of { (HappyWrap22 happy_var_3) -> 
	happyIn18
		 (Frontend.LambdaQ.Abs.TypeFunction happy_var_1 happy_var_3
	)}}

happyReduce_35 = happySpecReduce_1  14# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
	happyIn18
		 (happy_var_1
	)}

happyReduce_36 = happySpecReduce_3  15# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { (HappyWrap19 happy_var_1) -> 
	case happyOut20 happy_x_3 of { (HappyWrap20 happy_var_3) -> 
	happyIn19
		 (Frontend.LambdaQ.Abs.TypeTensorProd happy_var_1 happy_var_3
	)}}

happyReduce_37 = happySpecReduce_1  15# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	happyIn19
		 (happy_var_1
	)}

happyReduce_38 = happySpecReduce_3  16# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	case happyOut5 happy_x_3 of { (HappyWrap5 happy_var_3) -> 
	happyIn20
		 (Frontend.LambdaQ.Abs.TypeExp happy_var_1 happy_var_3
	)}}

happyReduce_39 = happySpecReduce_2  16# happyReduction_39
happyReduction_39 happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_2 of { (HappyWrap23 happy_var_2) -> 
	happyIn20
		 (Frontend.LambdaQ.Abs.TypeNonLinear happy_var_2
	)}

happyReduce_40 = happySpecReduce_1  16# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	happyIn20
		 (happy_var_1
	)}

happyReduce_41 = happySpecReduce_1  17# happyReduction_41
happyReduction_41 happy_x_1
	 =  happyIn21
		 (Frontend.LambdaQ.Abs.TypeBool
	)

happyReduce_42 = happySpecReduce_1  17# happyReduction_42
happyReduction_42 happy_x_1
	 =  happyIn21
		 (Frontend.LambdaQ.Abs.TypeBit
	)

happyReduce_43 = happySpecReduce_1  17# happyReduction_43
happyReduction_43 happy_x_1
	 =  happyIn21
		 (Frontend.LambdaQ.Abs.TypeInteger
	)

happyReduce_44 = happySpecReduce_1  17# happyReduction_44
happyReduction_44 happy_x_1
	 =  happyIn21
		 (Frontend.LambdaQ.Abs.TypeQbit
	)

happyReduce_45 = happySpecReduce_1  17# happyReduction_45
happyReduction_45 happy_x_1
	 =  happyIn21
		 (Frontend.LambdaQ.Abs.TypeUnit
	)

happyReduce_46 = happySpecReduce_3  17# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_2 of { (HappyWrap18 happy_var_2) -> 
	happyIn21
		 (Frontend.LambdaQ.Abs.TypeList happy_var_2
	)}

happyReduce_47 = happySpecReduce_3  17# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_2 of { (HappyWrap18 happy_var_2) -> 
	happyIn21
		 (happy_var_2
	)}

happyReduce_48 = happySpecReduce_1  18# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut19 happy_x_1 of { (HappyWrap19 happy_var_1) -> 
	happyIn22
		 (happy_var_1
	)}

happyReduce_49 = happySpecReduce_1  19# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut21 happy_x_1 of { (HappyWrap21 happy_var_1) -> 
	happyIn23
		 (happy_var_1
	)}

happyReduce_50 = happySpecReduce_1  20# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	happyIn24
		 (Frontend.LambdaQ.Abs.AngleValue happy_var_1
	)}

happyReduce_51 = happySpecReduce_1  21# happyReduction_51
happyReduction_51 happy_x_1
	 =  happyIn25
		 (Frontend.LambdaQ.Abs.BasisStateZero
	)

happyReduce_52 = happySpecReduce_1  21# happyReduction_52
happyReduction_52 happy_x_1
	 =  happyIn25
		 (Frontend.LambdaQ.Abs.BasisStateOne
	)

happyReduce_53 = happySpecReduce_1  21# happyReduction_53
happyReduction_53 happy_x_1
	 =  happyIn25
		 (Frontend.LambdaQ.Abs.BasisStatePlus
	)

happyReduce_54 = happySpecReduce_1  21# happyReduction_54
happyReduction_54 happy_x_1
	 =  happyIn25
		 (Frontend.LambdaQ.Abs.BasisStateMinus
	)

happyReduce_55 = happySpecReduce_1  21# happyReduction_55
happyReduction_55 happy_x_1
	 =  happyIn25
		 (Frontend.LambdaQ.Abs.BasisStatePlusI
	)

happyReduce_56 = happySpecReduce_1  21# happyReduction_56
happyReduction_56 happy_x_1
	 =  happyIn25
		 (Frontend.LambdaQ.Abs.BasisStateMinusI
	)

happyReduce_57 = happySpecReduce_1  22# happyReduction_57
happyReduction_57 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateH
	)

happyReduce_58 = happySpecReduce_1  22# happyReduction_58
happyReduction_58 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateX
	)

happyReduce_59 = happySpecReduce_1  22# happyReduction_59
happyReduction_59 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateY
	)

happyReduce_60 = happySpecReduce_1  22# happyReduction_60
happyReduction_60 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateZ
	)

happyReduce_61 = happySpecReduce_1  22# happyReduction_61
happyReduction_61 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateID
	)

happyReduce_62 = happySpecReduce_2  22# happyReduction_62
happyReduction_62 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateXRoot happy_var_2
	)}

happyReduce_63 = happySpecReduce_2  22# happyReduction_63
happyReduction_63 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateXRootDag happy_var_2
	)}

happyReduce_64 = happySpecReduce_2  22# happyReduction_64
happyReduction_64 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateYRoot happy_var_2
	)}

happyReduce_65 = happySpecReduce_2  22# happyReduction_65
happyReduction_65 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateYRootDag happy_var_2
	)}

happyReduce_66 = happySpecReduce_2  22# happyReduction_66
happyReduction_66 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateZRoot happy_var_2
	)}

happyReduce_67 = happySpecReduce_2  22# happyReduction_67
happyReduction_67 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateZRootDag happy_var_2
	)}

happyReduce_68 = happySpecReduce_1  22# happyReduction_68
happyReduction_68 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateS
	)

happyReduce_69 = happySpecReduce_1  22# happyReduction_69
happyReduction_69 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateSDag
	)

happyReduce_70 = happySpecReduce_1  22# happyReduction_70
happyReduction_70 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateT
	)

happyReduce_71 = happySpecReduce_1  22# happyReduction_71
happyReduction_71 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateTDag
	)

happyReduce_72 = happySpecReduce_1  22# happyReduction_72
happyReduction_72 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateSqrtX
	)

happyReduce_73 = happySpecReduce_1  22# happyReduction_73
happyReduction_73 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateSqrtXDag
	)

happyReduce_74 = happySpecReduce_1  22# happyReduction_74
happyReduction_74 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateSqrtY
	)

happyReduce_75 = happySpecReduce_1  22# happyReduction_75
happyReduction_75 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateSqrtYDag
	)

happyReduce_76 = happySpecReduce_2  22# happyReduction_76
happyReduction_76 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateRxTheta happy_var_2
	)}

happyReduce_77 = happySpecReduce_2  22# happyReduction_77
happyReduction_77 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateRyTheta happy_var_2
	)}

happyReduce_78 = happySpecReduce_2  22# happyReduction_78
happyReduction_78 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateRzTheta happy_var_2
	)}

happyReduce_79 = happySpecReduce_2  22# happyReduction_79
happyReduction_79 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateU1 happy_var_2
	)}

happyReduce_80 = happyReduce 6# 22# happyReduction_80
happyReduction_80 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
	case happyOut24 happy_x_5 of { (HappyWrap24 happy_var_5) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateU2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_81 = happyReduce 8# 22# happyReduction_81
happyReduction_81 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
	case happyOut24 happy_x_5 of { (HappyWrap24 happy_var_5) -> 
	case happyOut24 happy_x_7 of { (HappyWrap24 happy_var_7) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateU3 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_82 = happySpecReduce_1  22# happyReduction_82
happyReduction_82 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateSwp
	)

happyReduce_83 = happySpecReduce_1  22# happyReduction_83
happyReduction_83 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateSqrtSwp
	)

happyReduce_84 = happySpecReduce_1  22# happyReduction_84
happyReduction_84 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateSqrtSwpDag
	)

happyReduce_85 = happySpecReduce_1  22# happyReduction_85
happyReduction_85 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateISwp
	)

happyReduce_86 = happySpecReduce_1  22# happyReduction_86
happyReduction_86 happy_x_1
	 =  happyIn26
		 (Frontend.LambdaQ.Abs.GateFSwp
	)

happyReduce_87 = happySpecReduce_2  22# happyReduction_87
happyReduction_87 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateSwpTheta happy_var_2
	)}

happyReduce_88 = happySpecReduce_2  22# happyReduction_88
happyReduction_88 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateSwpRt happy_var_2
	)}

happyReduce_89 = happySpecReduce_2  22# happyReduction_89
happyReduction_89 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateSwpRtDag happy_var_2
	)}

happyReduce_90 = happySpecReduce_2  22# happyReduction_90
happyReduction_90 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateQft happy_var_2
	)}

happyReduce_91 = happySpecReduce_2  22# happyReduction_91
happyReduction_91 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateQftDag happy_var_2
	)}

happyReduce_92 = happyReduce 8# 22# happyReduction_92
happyReduction_92 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
	case happyOut24 happy_x_5 of { (HappyWrap24 happy_var_5) -> 
	case happyOut24 happy_x_7 of { (HappyWrap24 happy_var_7) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateUknown3Angle happy_var_1 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}}

happyReduce_93 = happyReduce 6# 22# happyReduction_93
happyReduction_93 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
	case happyOut24 happy_x_5 of { (HappyWrap24 happy_var_5) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateUknown2Angle happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_94 = happySpecReduce_2  22# happyReduction_94
happyReduction_94 happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateUknown1Angle happy_var_1 happy_var_2
	)}}

happyReduce_95 = happySpecReduce_2  22# happyReduction_95
happyReduction_95 happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateUknownInt happy_var_1 happy_var_2
	)}}

happyReduce_96 = happySpecReduce_1  22# happyReduction_96
happyReduction_96 happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	happyIn26
		 (Frontend.LambdaQ.Abs.GateUnknownSimple happy_var_1
	)}

happyReduce_97 = happySpecReduce_1  23# happyReduction_97
happyReduction_97 happy_x_1
	 =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	happyIn27
		 ((:[]) happy_var_1
	)}

happyReduce_98 = happySpecReduce_3  23# happyReduction_98
happyReduction_98 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	case happyOut27 happy_x_3 of { (HappyWrap27 happy_var_3) -> 
	happyIn27
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_99 = happySpecReduce_3  24# happyReduction_99
happyReduction_99 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_2 of { (HappyWrap25 happy_var_2) -> 
	happyIn28
		 (Frontend.LambdaQ.Abs.CtrlBasisState happy_var_2
	)}

happyReduce_100 = happyReduce 5# 25# happyReduction_100
happyReduction_100 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut25 happy_x_2 of { (HappyWrap25 happy_var_2) -> 
	case happyOut30 happy_x_4 of { (HappyWrap30 happy_var_4) -> 
	happyIn29
		 (Frontend.LambdaQ.Abs.CtrlBasisStates happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_101 = happySpecReduce_1  26# happyReduction_101
happyReduction_101 happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	happyIn30
		 ((:[]) happy_var_1
	)}

happyReduce_102 = happySpecReduce_3  26# happyReduction_102
happyReduction_102 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	case happyOut30 happy_x_3 of { (HappyWrap30 happy_var_3) -> 
	happyIn30
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_103 = happySpecReduce_3  27# happyReduction_103
happyReduction_103 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	happyIn31
		 (Frontend.LambdaQ.Abs.CtrlBit happy_var_2
	)}

happyReduce_104 = happyReduce 5# 28# happyReduction_104
happyReduction_104 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut5 happy_x_2 of { (HappyWrap5 happy_var_2) -> 
	case happyOut33 happy_x_4 of { (HappyWrap33 happy_var_4) -> 
	happyIn32
		 (Frontend.LambdaQ.Abs.CtrlBits happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_105 = happySpecReduce_1  29# happyReduction_105
happyReduction_105 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn33
		 ((:[]) happy_var_1
	)}

happyReduce_106 = happySpecReduce_3  29# happyReduction_106
happyReduction_106 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	case happyOut33 happy_x_3 of { (HappyWrap33 happy_var_3) -> 
	happyIn33
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_107 = happySpecReduce_3  30# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	happyIn34
		 (Frontend.LambdaQ.Abs.CtrlTerm happy_var_2
	)}

happyReduce_108 = happyReduce 5# 31# happyReduction_108
happyReduction_108 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	case happyOut38 happy_x_4 of { (HappyWrap38 happy_var_4) -> 
	happyIn35
		 (Frontend.LambdaQ.Abs.CtrlTerms happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_109 = happySpecReduce_3  32# happyReduction_109
happyReduction_109 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_2 of { (HappyWrap7 happy_var_2) -> 
	happyIn36
		 (Frontend.LambdaQ.Abs.CtrlVar happy_var_2
	)}

happyReduce_110 = happyReduce 5# 33# happyReduction_110
happyReduction_110 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { (HappyWrap7 happy_var_2) -> 
	case happyOut27 happy_x_4 of { (HappyWrap27 happy_var_4) -> 
	happyIn37
		 (Frontend.LambdaQ.Abs.CtrlVars happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_111 = happySpecReduce_1  34# happyReduction_111
happyReduction_111 happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	happyIn38
		 ((:[]) happy_var_1
	)}

happyReduce_112 = happySpecReduce_3  34# happyReduction_112
happyReduction_112 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	case happyOut38 happy_x_3 of { (HappyWrap38 happy_var_3) -> 
	happyIn38
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_113 = happySpecReduce_3  35# happyReduction_113
happyReduction_113 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	case happyOut5 happy_x_3 of { (HappyWrap5 happy_var_3) -> 
	happyIn39
		 (Frontend.LambdaQ.Abs.TermListElement happy_var_1 happy_var_3
	)}}

happyReduce_114 = happySpecReduce_3  35# happyReduction_114
happyReduction_114 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	happyIn39
		 (happy_var_2
	)}

happyReduce_115 = happySpecReduce_1  36# happyReduction_115
happyReduction_115 happy_x_1
	 =  happyIn40
		 (Frontend.LambdaQ.Abs.TermUnit
	)

happyReduce_116 = happySpecReduce_1  36# happyReduction_116
happyReduction_116 happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	happyIn40
		 (Frontend.LambdaQ.Abs.TermBasisState happy_var_1
	)}

happyReduce_117 = happySpecReduce_1  36# happyReduction_117
happyReduction_117 happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	happyIn40
		 (Frontend.LambdaQ.Abs.TermBoolExpression happy_var_1
	)}

happyReduce_118 = happySpecReduce_1  36# happyReduction_118
happyReduction_118 happy_x_1
	 =  case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
	happyIn40
		 (Frontend.LambdaQ.Abs.TermIntegerExpression happy_var_1
	)}

happyReduce_119 = happySpecReduce_2  36# happyReduction_119
happyReduction_119 happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_2 of { (HappyWrap26 happy_var_2) -> 
	happyIn40
		 (Frontend.LambdaQ.Abs.TermGate happy_var_2
	)}

happyReduce_120 = happySpecReduce_1  36# happyReduction_120
happyReduction_120 happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	happyIn40
		 (Frontend.LambdaQ.Abs.TermList happy_var_1
	)}

happyReduce_121 = happySpecReduce_1  36# happyReduction_121
happyReduction_121 happy_x_1
	 =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	happyIn40
		 (Frontend.LambdaQ.Abs.TermVariable happy_var_1
	)}

happyReduce_122 = happyReduce 5# 36# happyReduction_122
happyReduction_122 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	case happyOut38 happy_x_4 of { (HappyWrap38 happy_var_4) -> 
	happyIn40
		 (Frontend.LambdaQ.Abs.TermTuple happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_123 = happySpecReduce_1  36# happyReduction_123
happyReduction_123 happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	happyIn40
		 (happy_var_1
	)}

happyReduce_124 = happyReduce 4# 37# happyReduction_124
happyReduction_124 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut34 happy_x_2 of { (HappyWrap34 happy_var_2) -> 
	case happyOut28 happy_x_4 of { (HappyWrap28 happy_var_4) -> 
	happyIn41
		 (Frontend.LambdaQ.Abs.TermQuantumCtrlGate happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_125 = happyReduce 4# 37# happyReduction_125
happyReduction_125 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut35 happy_x_2 of { (HappyWrap35 happy_var_2) -> 
	case happyOut29 happy_x_4 of { (HappyWrap29 happy_var_4) -> 
	happyIn41
		 (Frontend.LambdaQ.Abs.TermQuantumTCtrlsGate happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_126 = happyReduce 4# 37# happyReduction_126
happyReduction_126 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_2 of { (HappyWrap37 happy_var_2) -> 
	case happyOut29 happy_x_4 of { (HappyWrap29 happy_var_4) -> 
	happyIn41
		 (Frontend.LambdaQ.Abs.TermQuantumVCtrlsGate happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_127 = happyReduce 4# 37# happyReduction_127
happyReduction_127 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut34 happy_x_2 of { (HappyWrap34 happy_var_2) -> 
	case happyOut31 happy_x_4 of { (HappyWrap31 happy_var_4) -> 
	happyIn41
		 (Frontend.LambdaQ.Abs.TermClassicCtrlGate happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_128 = happyReduce 4# 37# happyReduction_128
happyReduction_128 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut35 happy_x_2 of { (HappyWrap35 happy_var_2) -> 
	case happyOut32 happy_x_4 of { (HappyWrap32 happy_var_4) -> 
	happyIn41
		 (Frontend.LambdaQ.Abs.TermClassicTCtrlsGate happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_129 = happyReduce 4# 37# happyReduction_129
happyReduction_129 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_2 of { (HappyWrap37 happy_var_2) -> 
	case happyOut32 happy_x_4 of { (HappyWrap32 happy_var_4) -> 
	happyIn41
		 (Frontend.LambdaQ.Abs.TermClassicVCtrlsGate happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_130 = happySpecReduce_2  37# happyReduction_130
happyReduction_130 happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	case happyOut40 happy_x_2 of { (HappyWrap40 happy_var_2) -> 
	happyIn41
		 (Frontend.LambdaQ.Abs.TermApply happy_var_1 happy_var_2
	)}}

happyReduce_131 = happySpecReduce_3  37# happyReduction_131
happyReduction_131 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	case happyOut40 happy_x_3 of { (HappyWrap40 happy_var_3) -> 
	happyIn41
		 (Frontend.LambdaQ.Abs.TermCompose happy_var_1 happy_var_3
	)}}

happyReduce_132 = happySpecReduce_3  37# happyReduction_132
happyReduction_132 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	case happyOut40 happy_x_3 of { (HappyWrap40 happy_var_3) -> 
	happyIn41
		 (Frontend.LambdaQ.Abs.TermTensorProduct happy_var_1 happy_var_3
	)}}

happyReduce_133 = happySpecReduce_1  37# happyReduction_133
happyReduction_133 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn41
		 (happy_var_1
	)}

happyReduce_134 = happyReduce 6# 38# happyReduction_134
happyReduction_134 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	case happyOut43 happy_x_4 of { (HappyWrap43 happy_var_4) -> 
	case happyOut43 happy_x_6 of { (HappyWrap43 happy_var_6) -> 
	happyIn42
		 (Frontend.LambdaQ.Abs.TermIfElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_135 = happyReduce 8# 38# happyReduction_135
happyReduction_135 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_3 of { (HappyWrap7 happy_var_3) -> 
	case happyOut43 happy_x_5 of { (HappyWrap43 happy_var_5) -> 
	case happyOut43 happy_x_8 of { (HappyWrap43 happy_var_8) -> 
	happyIn42
		 (Frontend.LambdaQ.Abs.TermLetSingle happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest}}}

happyReduce_136 = happyReduce 12# 38# happyReduction_136
happyReduction_136 (happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_4 of { (HappyWrap7 happy_var_4) -> 
	case happyOut27 happy_x_6 of { (HappyWrap27 happy_var_6) -> 
	case happyOut43 happy_x_9 of { (HappyWrap43 happy_var_9) -> 
	case happyOut43 happy_x_12 of { (HappyWrap43 happy_var_12) -> 
	happyIn42
		 (Frontend.LambdaQ.Abs.TermLetMultiple happy_var_4 happy_var_6 happy_var_9 happy_var_12
	) `HappyStk` happyRest}}}}

happyReduce_137 = happyReduce 5# 38# happyReduction_137
happyReduction_137 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	case happyOut43 happy_x_3 of { (HappyWrap43 happy_var_3) -> 
	case happyOut43 happy_x_5 of { (HappyWrap43 happy_var_5) -> 
	happyIn42
		 (Frontend.LambdaQ.Abs.TermLetSugarSingle happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_138 = happyReduce 9# 38# happyReduction_138
happyReduction_138 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_2 of { (HappyWrap7 happy_var_2) -> 
	case happyOut27 happy_x_4 of { (HappyWrap27 happy_var_4) -> 
	case happyOut43 happy_x_7 of { (HappyWrap43 happy_var_7) -> 
	case happyOut43 happy_x_9 of { (HappyWrap43 happy_var_9) -> 
	happyIn42
		 (Frontend.LambdaQ.Abs.TermLetSugarMultiple happy_var_2 happy_var_4 happy_var_7 happy_var_9
	) `HappyStk` happyRest}}}}

happyReduce_139 = happyReduce 6# 38# happyReduction_139
happyReduction_139 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	case happyOut47 happy_x_5 of { (HappyWrap47 happy_var_5) -> 
	happyIn42
		 (Frontend.LambdaQ.Abs.TermCase happy_var_2 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_140 = happyReduce 5# 38# happyReduction_140
happyReduction_140 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	case happyOut7 happy_x_2 of { (HappyWrap7 happy_var_2) -> 
	case happyOut18 happy_x_3 of { (HappyWrap18 happy_var_3) -> 
	case happyOut43 happy_x_5 of { (HappyWrap43 happy_var_5) -> 
	happyIn42
		 (Frontend.LambdaQ.Abs.TermLambda happy_var_1 happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}}

happyReduce_141 = happySpecReduce_1  38# happyReduction_141
happyReduction_141 happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	happyIn42
		 (happy_var_1
	)}

happyReduce_142 = happySpecReduce_3  39# happyReduction_142
happyReduction_142 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	case happyOut43 happy_x_3 of { (HappyWrap43 happy_var_3) -> 
	happyIn43
		 (Frontend.LambdaQ.Abs.TermDollar happy_var_1 happy_var_3
	)}}

happyReduce_143 = happySpecReduce_1  39# happyReduction_143
happyReduction_143 happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	happyIn43
		 (happy_var_1
	)}

happyReduce_144 = happySpecReduce_1  40# happyReduction_144
happyReduction_144 happy_x_1
	 =  happyIn44
		 (Frontend.LambdaQ.Abs.ListNil
	)

happyReduce_145 = happySpecReduce_3  40# happyReduction_145
happyReduction_145 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	happyIn44
		 (Frontend.LambdaQ.Abs.ListSingle happy_var_2
	)}

happyReduce_146 = happyReduce 5# 40# happyReduction_146
happyReduction_146 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	case happyOut38 happy_x_4 of { (HappyWrap38 happy_var_4) -> 
	happyIn44
		 (Frontend.LambdaQ.Abs.ListMultiple happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_147 = happySpecReduce_3  40# happyReduction_147
happyReduction_147 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_2 of { (HappyWrap45 happy_var_2) -> 
	happyIn44
		 (happy_var_2
	)}

happyReduce_148 = happySpecReduce_3  41# happyReduction_148
happyReduction_148 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	case happyOut44 happy_x_3 of { (HappyWrap44 happy_var_3) -> 
	happyIn45
		 (Frontend.LambdaQ.Abs.ListExpressionAdd happy_var_1 happy_var_3
	)}}

happyReduce_149 = happySpecReduce_3  41# happyReduction_149
happyReduction_149 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	case happyOut44 happy_x_3 of { (HappyWrap44 happy_var_3) -> 
	happyIn45
		 (Frontend.LambdaQ.Abs.ListCons happy_var_1 happy_var_3
	)}}

happyReduce_150 = happySpecReduce_1  41# happyReduction_150
happyReduction_150 happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	happyIn45
		 (happy_var_1
	)}

happyReduce_151 = happySpecReduce_3  42# happyReduction_151
happyReduction_151 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	case happyOut43 happy_x_3 of { (HappyWrap43 happy_var_3) -> 
	happyIn46
		 (Frontend.LambdaQ.Abs.CaseExpr happy_var_1 happy_var_3
	)}}

happyReduce_152 = happySpecReduce_2  42# happyReduction_152
happyReduction_152 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	happyIn46
		 (happy_var_1
	)}

happyReduce_153 = happySpecReduce_1  43# happyReduction_153
happyReduction_153 happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	happyIn47
		 ((:[]) happy_var_1
	)}

happyReduce_154 = happySpecReduce_2  43# happyReduction_154
happyReduction_154 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	happyIn47
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_155 = happySpecReduce_1  44# happyReduction_155
happyReduction_155 happy_x_1
	 =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	happyIn48
		 (Frontend.LambdaQ.Abs.FunArg happy_var_1
	)}

happyReduce_156 = happySpecReduce_0  45# happyReduction_156
happyReduction_156  =  happyIn49
		 ([]
	)

happyReduce_157 = happySpecReduce_2  45# happyReduction_157
happyReduction_157 happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	case happyOut49 happy_x_2 of { (HappyWrap49 happy_var_2) -> 
	happyIn49
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_158 = happyReduce 4# 46# happyReduction_158
happyReduction_158 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	case happyOut49 happy_x_2 of { (HappyWrap49 happy_var_2) -> 
	case happyOut43 happy_x_4 of { (HappyWrap43 happy_var_4) -> 
	happyIn50
		 (Frontend.LambdaQ.Abs.FunDef happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_159 = happySpecReduce_2  46# happyReduction_159
happyReduction_159 happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	happyIn50
		 (happy_var_1
	)}

happyReduce_160 = happySpecReduce_3  47# happyReduction_160
happyReduction_160 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	case happyOut18 happy_x_3 of { (HappyWrap18 happy_var_3) -> 
	happyIn51
		 (Frontend.LambdaQ.Abs.FunType happy_var_1 happy_var_3
	)}}

happyReduce_161 = happySpecReduce_2  47# happyReduction_161
happyReduction_161 happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	happyIn51
		 (happy_var_1
	)}

happyReduce_162 = happyReduce 4# 48# happyReduction_162
happyReduction_162 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	case happyOut50 happy_x_3 of { (HappyWrap50 happy_var_3) -> 
	happyIn52
		 (Frontend.LambdaQ.Abs.FunDecl happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_163 = happySpecReduce_0  49# happyReduction_163
happyReduction_163  =  happyIn53
		 ([]
	)

happyReduce_164 = happySpecReduce_2  49# happyReduction_164
happyReduction_164 happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	case happyOut53 happy_x_2 of { (HappyWrap53 happy_var_2) -> 
	happyIn53
		 ((:) happy_var_1 happy_var_2
	)}}

happyNewToken action sts stk [] =
	happyDoAction 98# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TS _ 46) -> cont 46#;
	PT _ (TS _ 47) -> cont 47#;
	PT _ (TS _ 48) -> cont 48#;
	PT _ (TS _ 49) -> cont 49#;
	PT _ (TS _ 50) -> cont 50#;
	PT _ (TS _ 51) -> cont 51#;
	PT _ (TS _ 52) -> cont 52#;
	PT _ (TS _ 53) -> cont 53#;
	PT _ (TS _ 54) -> cont 54#;
	PT _ (TS _ 55) -> cont 55#;
	PT _ (TS _ 56) -> cont 56#;
	PT _ (TS _ 57) -> cont 57#;
	PT _ (TS _ 58) -> cont 58#;
	PT _ (TS _ 59) -> cont 59#;
	PT _ (TS _ 60) -> cont 60#;
	PT _ (TS _ 61) -> cont 61#;
	PT _ (TS _ 62) -> cont 62#;
	PT _ (TS _ 63) -> cont 63#;
	PT _ (TS _ 64) -> cont 64#;
	PT _ (TS _ 65) -> cont 65#;
	PT _ (TS _ 66) -> cont 66#;
	PT _ (TS _ 67) -> cont 67#;
	PT _ (TS _ 68) -> cont 68#;
	PT _ (TS _ 69) -> cont 69#;
	PT _ (TS _ 70) -> cont 70#;
	PT _ (TS _ 71) -> cont 71#;
	PT _ (TS _ 72) -> cont 72#;
	PT _ (TS _ 73) -> cont 73#;
	PT _ (TS _ 74) -> cont 74#;
	PT _ (TS _ 75) -> cont 75#;
	PT _ (TS _ 76) -> cont 76#;
	PT _ (TS _ 77) -> cont 77#;
	PT _ (TS _ 78) -> cont 78#;
	PT _ (TS _ 79) -> cont 79#;
	PT _ (TS _ 80) -> cont 80#;
	PT _ (TS _ 81) -> cont 81#;
	PT _ (TS _ 82) -> cont 82#;
	PT _ (TS _ 83) -> cont 83#;
	PT _ (TS _ 84) -> cont 84#;
	PT _ (TS _ 85) -> cont 85#;
	PT _ (TS _ 86) -> cont 86#;
	PT _ (TS _ 87) -> cont 87#;
	PT _ (TS _ 88) -> cont 88#;
	PT _ (TS _ 89) -> cont 89#;
	PT _ (TS _ 90) -> cont 90#;
	PT _ (TS _ 91) -> cont 91#;
	PT _ (TS _ 92) -> cont 92#;
	PT _ (TD happy_dollar_dollar) -> cont 93#;
	PT _ (TI happy_dollar_dollar) -> cont 94#;
	PT _ (T_GateVar _) -> cont 95#;
	PT _ (T_Var _) -> cont 96#;
	PT _ (T_Lambda happy_dollar_dollar) -> cont 97#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 98# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap9 x') = happyOut9 x} in x'))

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
