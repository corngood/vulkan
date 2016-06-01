{-# LANGUAGE QuasiQuotes #-}

module Write.Command
  ( writeCommand
  ) where

import           Data.List
import           Language.C.Types
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.Syntax  hiding (ModuleName)
import           Spec.Command
import           Spec.Type                     (CType)
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  hiding ((<$>))
import           Write.TypeConverter
import           Write.Utils
import           Write.WriteMonad

writeCommand :: Command -> Write Doc
writeCommand c = do
  let symbol = unCIdentifier (cSymbol c)
      name = cHsName c
  commandType <- writeCommandType c
  -- TODO: obviously this is not cool
  if "EXT" `isSuffixOf` symbol
     then do tellRequiredName (ExternalName (ModuleName "Foreign.Ptr") "FunPtr")
             tellRequiredName (ExternalName (ModuleName "Foreign.Ptr") "castFunPtr")
             tellRequiredName (ExternalName (ModuleName "Foreign.C.String") "withCString")
             tellRequiredName (ExternalName (ModuleName "System.IO.Unsafe") "unsafePerformIO")
             tellRequiredName (ExternalName (ModuleName (rawModuleBase ++ ".DeviceInitialization")) "getInstanceProcAddr")
             -- TODO: look for instance/device/other commands and fetch appropriately
             pure [qc|-- ** {name}
foreign import ccall "dynamic" mk{symbol} :: FunPtr ({commandType}) -> ({commandType})
{name} :: {commandType}
{name} i = (mk{symbol} $ castFunPtr $ procAddr) i
  where procAddr = unsafePerformIO $ withCString "{symbol}" $ getInstanceProcAddr i
|]
    else pure [qc|-- ** {name}
foreign import ccall "{symbol}" {name} ::
  {commandType}
|]

writeCommandType :: Command -> Write String
writeCommandType c = do
  hsReturnType <- (simpleCon "IO" `TyApp`) <$> cTypeToHsType (cReturnType c)
  hsParameterTypes <- traverse (cTypeToHsType . lowerArrayToPointer)
                              (pType <$> cParameters c)
  let hsType = foldr TyFun hsReturnType hsParameterTypes
  pure $ prettyPrint hsType

lowerArrayToPointer :: CType -> CType
lowerArrayToPointer cType =
  case cType of
    Array _ t -> Ptr [] t
    t -> t
