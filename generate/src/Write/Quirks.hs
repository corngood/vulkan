module Write.Quirks where

import           Data.HashMap.Strict as M
import           Write.Utils

-- | Entities which must be put in hs-boot files to break dependency cycles
--
-- Only handles are allowed in here, this isn't checked
cycleBreakers :: HashMap ModuleName [String]
cycleBreakers = M.fromList [ (ModuleName (rawModuleBase ++ ".Device"), ["VkDevice"])
                           , (ModuleName (rawModuleBase ++ ".Pass"), ["VkRenderPass"])
                           ]

sourceImports :: HashMap ModuleName [ModuleName]
sourceImports = M.fromList [ ( ModuleName (rawModuleBase ++ ".Memory")
                             , [ModuleName (rawModuleBase ++ ".Device")]
                             )
                           , ( ModuleName (rawModuleBase ++ ".Pipeline")
                             , [ModuleName (rawModuleBase ++ ".Pass")]
                             )
                           ]


