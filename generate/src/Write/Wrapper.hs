{-# LANGUAGE QuasiQuotes #-}

module Write.Wrapper where

import qualified Data.HashMap.Lazy             as Map
import           Data.String
import           Spec.Graph                    as G
import           Spec.Type                     as T
import           Spec.TypeEnv
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  hiding ((<$>))
import           Write.TypeConverter
import           Write.Utils
import           Write.WriteMonad

writeWrapperModule :: SpecGraph -> Write Doc
writeWrapperModule graph = contents
  where contents = writeWrapper $ vSourceEntity $ requiredLookup graph "VkApplicationInfo"

rawName :: String -> String -> RequiredName
rawName m n = QualifiedName (ModuleName $ rawModuleBase ++ "." ++ m) "Vk" n

writeWrapper :: SourceEntity -> Write Doc
writeWrapper (G.AStructType s) = do
  typeMap <- teNameLocations <$> askTypeEnv
  inner <- case Map.lookup (stName s) typeMap of
    Just (m, n) -> do
      tellRequiredName (QualifiedName m "Vk" $ n ++ "(..)")
      pure $ n
    Nothing ->
      pure $ stName s
  let name = stHsName s
      writeMemberCons :: StructMember -> Write ([Doc], Doc)
      writeMemberCons sm | smName sm == "sType" = do
                             let st = "StructureType" ++ inner
                             tellExtension "PatternSynonyms"
                             tellRequiredName $ rawName "Core" ("pattern " ++ st)
                             pure ([], fromString $ "Vk." ++ st)
      writeMemberCons sm | smName sm == "pNext" = do
                             tellRequiredName (ExternalName (ModuleName "Foreign.Ptr") "nullPtr")
                             pure ([], fromString "nullPtr")
      writeMemberCons sm = do
        member <- writeMember sm
        pure ([member], [qc|({smHsName sm} i)|])
  memberInfo <- traverse writeMemberCons $ stMembers s
  let members = concat $ fst <$> memberInfo
      wrapCons = snd <$> memberInfo
  tellRequiredName (ExternalName (ModuleName "Foreign.Marshal") "with")
  pure [qc|
data {name} = {name} \{
{vsep $ intercalatePrepend (fromString ",") members}
}

wrap :: {name} -> (Ptr Vk.{inner} -> IO a) -> IO a
wrap i f = with (Vk.{inner} {hsep wrapCons}) f|]

writeWrapper _ = pure [qc||]

writeMember :: StructMember -> Write Doc
writeMember sm = do
  hsType <- cTypeToHsTypeString (smCType sm)
  pure ([qc|{smHsName sm} :: {hsType}|] :: Doc)
