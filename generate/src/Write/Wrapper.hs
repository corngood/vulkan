{-# LANGUAGE QuasiQuotes #-}

module Write.Wrapper where

import           Data.String
import           Spec.Graph                    as G
import           Spec.Type                     as T
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  hiding ((<$>))
import           Write.TypeConverter
import           Write.Utils
import           Write.WriteMonad

writeWrapperModule :: SpecGraph -> Write Doc
writeWrapperModule graph = contents
  where contents = writeWrapper $ vSourceEntity $ requiredLookup graph "VkApplicationInfo"

writeWrapper :: SourceEntity -> Write Doc
writeWrapper (G.AStructType s) = do
  members <- traverse writeMember $ stMembers s
  let name = stName s
  pure [qc|data {name} = {name} \{ {vsep $ intercalatePrepend (fromString ",") members} }|]
writeWrapper _ = pure [qc||]

writeMember :: StructMember -> Write Doc
writeMember sm = do
  hsType <- cTypeToHsTypeString (smCType sm)
  pure ([qc|{smName sm} :: {hsType}|] :: Doc)
