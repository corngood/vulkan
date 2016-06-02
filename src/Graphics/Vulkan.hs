
module Graphics.Vulkan where

import Data.Word( Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Raw.Core( StructureType(..)
                               )
import Foreign.C.Types( CChar(..)
                      )

data ApplicationInfo = ApplicationInfo { sType :: StructureType
, pNext :: Ptr Void
, pApplicationName :: Ptr CChar
, applicationVersion :: Word32
, pEngineName :: Ptr CChar
, engineVersion :: Word32
, apiVersion :: Word32 }
