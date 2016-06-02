module Write.CycleBreak where

import           Control.Monad       (void)
import           Data.HashMap.Strict as M
import           Spec.Graph
import           Write.Module
import           Write.Quirks
import           Write.TypeConverter (TypeEnv)
import           Write.Utils
import           Write.Vertex
import           Write.WriteMonad

writeHsBootFiles :: FilePath -> SpecGraph -> TypeEnv -> IO ()
writeHsBootFiles root graph typeEnv =
  void $ M.traverseWithKey (writeHsBootFile root typeEnv)
    (fmap (vSourceEntity . requiredLookup graph) <$> cycleBreakers)

writeHsBootFile :: FilePath      -- ^ The source root
                -> TypeEnv
                -> ModuleName    -- ^ The module name we're writing
                -> [SourceEntity]      -- ^ The symbols to export
                -> IO ()
writeHsBootFile root typeEnv moduleName exports = do
  createModuleDirectory root moduleName
  let moduleString = writeModule typeEnv Boot moduleName (writeVertices exports)
  writeFile (moduleNameToFile root moduleName ++ "-boot") moduleString

