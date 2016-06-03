{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE QuasiQuotes       #-}

module Write.Module
  ( writeModule
  )where

import           Data.Hashable
import           Data.HashMap.Strict           as M
import           Data.HashSet                  as S
import           Data.Maybe                    (catMaybes)
import           Data.String
import           GHC.Generics                  (Generic)
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  hiding ((<$>))
import           Write.Quirks
import           Write.TypeConverter           (TypeEnv)
import           Write.Utils
import           Write.WriteMonad

writeModule :: TypeEnv
            -> FileType
            -> ModuleName
            -> Write Doc
            -> String
writeModule typeEnv boot (ModuleName n) writeDefinitions = moduleString
  where (moduleString, (extraRequiredNames, extensions)) =
          runWrite typeEnv boot moduleWriter
        extensionDocs = getExtensionDoc <$> S.toList extensions
        requiredNames = extraRequiredNames
        imports = vcat (getImportDeclarations (ModuleName n) requiredNames)
        moduleWriter = do
          definitions <- writeDefinitions
          pure [qc|{vcat extensionDocs}
module {n} where

{imports}

{definitions}
|]

getExtensionDoc :: String -> Doc
getExtensionDoc e = let ed = fromString e :: Doc
                       in [qc|\{-# LANGUAGE {ed} #-}|]

-- nameToRequiredName :: SpecGraph -> String -> RequiredName
-- nameToRequiredName graph name =
--   case name of
--     "int32_t"  -> ExternalName (ModuleName "Data.Int")  "Int32"
--     "uint8_t"  -> ExternalName (ModuleName "Data.Word") "Word8"
--     "uint32_t" -> ExternalName (ModuleName "Data.Word") "Word32"
--     "uint64_t" -> ExternalName (ModuleName "Data.Word") "Word64"
--     "size_t"   -> ExternalName (ModuleName "Foreign.C.Types") "CSize(..)"
--     "void"     -> ExternalName (ModuleName "Data.Void") "Void"
--     "float"    -> ExternalName (ModuleName "Foreign.C.Types") "CFloat(..)"
--     _ -> if isTypeConstructor (requiredLookup graph name)
--            then InternalName WildCard name
--            else InternalName NoWildCard name

getImportDeclarations :: ModuleName -> HashSet RequiredName -> [Doc]
getImportDeclarations importingModule names =
    fmap (writeImport . makeImportSourcy importingModule) .
      mergeImports $ imports
  where imports = catMaybes (getImportDeclaration <$> S.toList names)
        getImportDeclaration rn =
          case rn of
            ExternalName moduleName name | moduleName /= importingModule ->
              Just (Import NotSource (ImportModule False moduleName Nothing) [name])
            QualifiedName moduleName asName name | moduleName /= importingModule ->
              Just (Import NotSource (ImportModule True moduleName (Just asName)) [name])
            _ -> Nothing

data ImportModule = ImportModule Bool ModuleName (Maybe String)
                    deriving(Eq, Generic)

instance Hashable ImportModule

data Import = Import Source ImportModule [String]

data Source = NotSource
            | Source

writeImport :: Import -> Doc
writeImport (Import source (ImportModule qualified (ModuleName moduleName) asName) imports) =
  let sourceDoc :: Doc
      qual :: String
      qual = if qualified then "qualified " else ""
      as = maybe "" (\x -> " as " ++ x ++ " ") asName
      sourceDoc = fromString $ case source of
                                 NotSource -> ""
                                 Source -> "{-# SOURCE #-} "
  in [qc|import {qual}{sourceDoc}{moduleName}{as}( {indent (-2) (vcat ((intercalatePrepend "," (fromString <$> imports) ++ [")"])))}|]

mergeImports :: [Import] -> [Import]
mergeImports is = fmap (uncurry (Import NotSource)) .
                  M.toList . M.fromListWith (++) $
                  [(name, imports) | Import _ name imports <- is]

makeImportSourcy :: ModuleName -> Import -> Import
makeImportSourcy importingModule (Import source (ImportModule qualified moduleName as) names)
  | Just sourceImported <- M.lookup importingModule sourceImports
  , moduleName `elem` sourceImported
  = Import Source (ImportModule qualified moduleName as) names
  | otherwise
  = Import source (ImportModule qualified moduleName as) names
