{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TupleSections #-}

module Write.Spec
  ( writeSpecModules
  ) where

import           Spec.Spec
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  (Doc, indent, vcat, (<+>))

import           Control.Arrow                 (second)
import           Data.Foldable                 (traverse_)
import qualified Data.HashMap.Strict           as M
import qualified Data.HashSet                  as S
import           Data.List                     (sort)
import           Data.String
import           Spec.Graph
import           Spec.Partition
import           Write.CycleBreak
import           Write.Module
import           Write.Utils
import           Write.WriteMonad

writeSpecModules :: FilePath -> Spec -> IO ()
writeSpecModules root spec = do
  let graph = getSpecGraph spec
      partitions = second S.toList <$> M.toList (moduleExports (partitionSpec spec graph))
      locations = M.unions (uncurry exportMap <$> partitions)
      moduleNames = fst <$> partitions
      moduleStrings = uncurry (writeModule graph locations Normal) <$>
                      partitions
      modules = zip moduleNames moduleStrings
  traverse_ (createModuleDirectory root) (fst <$> modules)
  mapM_ (uncurry (writeModuleFile root)) modules
  writeHsBootFiles root graph locations
  writeModuleFile root (ModuleName "Graphics.Vulkan.Raw")
                       (writeParentModule $ fst <$> modules)

writeModuleFile :: FilePath -> ModuleName -> String -> IO ()
writeModuleFile root moduleName =
  writeFile (moduleNameToFile root moduleName)

exportMap :: ModuleName -> [String] -> M.HashMap String ModuleName
exportMap moduleName exports = M.fromList ((,moduleName) <$> exports)

writeParentModule :: [ModuleName] -> String
writeParentModule names = show moduleDoc
  where nameStrings = fmap fromString . sort . fmap unModuleName $ names
        moduleDoc :: Doc
        moduleDoc = [qc|module Graphics.Vulkan.Raw
  ( {indent (-2) . vcat $ intercalatePrepend (fromString ",") ((fromString "module" <+>) <$> nameStrings)}
  ) where

{vcat $ (fromString "import" <+>) <$> nameStrings}|]
