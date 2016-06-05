{-# LANGUAGE QuasiQuotes #-}

module Write.Wrapper where

-- import           Data.Char                     (isUpper, toLower, toUpper)
import           Data.Char                     (isUpper, toUpper)
import qualified Data.HashMap.Lazy             as Map
import           Data.List                     (isSuffixOf)
import           Data.String
import           Spec.Graph                    as G
import           Spec.Type                     as T
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  hiding ((<$>))
import qualified Text.PrettyPrint.Leijen.Text  as PP
import           Write.TypeConverter
import           Write.Utils
import           Write.WriteMonad

rawName :: String -> String -> RequiredName
rawName m = ExternalName (ModuleName $ rawModuleBase ++ "." ++ m)

writeWrapperModule :: NameLocations -> SpecGraph -> Write Doc
writeWrapperModule typeMap graph = (PP.<$>) <$> header <*> contents
  where contents = writeWrapper typeMap $ vSourceEntity $ requiredLookup graph "VkApplicationInfo"
        header = do
          tellExtension "MultiParamTypeClasses"
          tellExtension "FunctionalDependencies"
          tellExtension "TypeSynonymInstances"
          tellExtension "FlexibleInstances"
          tellRequiredName (ExternalName (ModuleName "Foreign.C.String") "CString")
          tellRequiredName (ExternalName (ModuleName "Foreign.C.String") "withCString")
          tellRequiredName $ rawName "Version" "vkMakeVersion"
          tellRequiredName $ rawName "Core" "VkStructureType"
          pure [qc|
data Version = Version Int Int Int

class WithVk a b | a -> b where
  withVk :: a -> (b -> IO c) -> IO c

instance WithVk String CString where
  withVk = withCString

instance WithVk Version Word32 where
  withVk (Version a b c) f = f v
    where v = vkMakeVersion (fromIntegral a) (fromIntegral b) (fromIntegral c)

instance WithVk VkStructureType VkStructureType where
  withVk a f = f a

instance WithVk (Ptr Void) (Ptr Void) where
  withVk a f = f a

wrapValue :: (WithVk a b) => a -> (c -> IO d) -> (b -> c) -> IO d
wrapValue a g f = undefined
|]

pascalToUnderscore :: String -> String
pascalToUnderscore [] = []
pascalToUnderscore (x:xs) = toUpper x : f xs
  where f (y:ys) | isUpper y = '_' : y : f ys
        f (y:ys) = toUpper y : f ys
        f [] = []

structureType :: String -> String
structureType = ("VK_STRUCTURE_TYPE_" ++) . pascalToUnderscore . dropVK

writeWrapper :: NameLocations -> SourceEntity -> Write Doc
writeWrapper typeMap (G.AStructType s) = do
  let inner = stName s
      name = dropVK inner
  case Map.lookup (stName s) typeMap of
    Just m -> tellRequiredName (ExternalName m $ inner ++ "(..)")
    Nothing -> pure ()
  let writeMemberCons :: [StructMember] -> Write ([Doc], Doc)
      writeMemberCons [] = pure ([], [qc|f|])
      writeMemberCons (sm:next) = do
        (am, ac) <- writeMemberValue sm
        (bm, bc) <- writeMemberCons next
        pure (am ++ bm, [qc|wrapValue {ac} ({bc})|])
      writeMemberValue :: StructMember -> Write ([Doc], Doc)
      writeMemberValue sm | smName sm == "sType" = do
                              let st = structureType inner
                              tellExtension "PatternSynonyms"
                              tellRequiredName $ rawName "Core" ("pattern " ++ st)
                              pure ([], fromString $ st)
      writeMemberValue sm | smName sm == "pNext" = do
                              tellRequiredName (ExternalName (ModuleName "Foreign.Ptr") "nullPtr")
                              tellRequiredName (ExternalName (ModuleName "Data.Void") "Void")
                              pure ([], fromString "(nullPtr :: Ptr Void)")
      writeMemberValue sm = do
        hsType <- cTypeToHsTypeString (smCType sm)
        let mName = smName sm
            wrapType "Ptr CChar" = "String"
            wrapType "Word32" | "Version" `isSuffixOf` mName = "Version"
            wrapType x = x
            -- wrapName ('p':x:xs) | isUpper x = toLower x:xs
            wrapName n = n
            name' = wrapName mName
        pure ([[qc|{name'} :: {wrapType hsType}|]], [qc|({name'} i)|])
  (members, cons) <- writeMemberCons $ stMembers s
  tellRequiredName (ExternalName (ModuleName "Foreign.Marshal") "with")
  pure [qc|
data {name} = {name} \{
{vsep $ intercalatePrepend (fromString ",") members}
}

instance WithVk {name} {inner} where
  withVk i f = {cons} {inner}|]

writeWrapper _ _ = pure [qc||]
