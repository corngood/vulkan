
module Graphics.Vulkan where

import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  )
import Data.Void( Void
                )
import Foreign.C.Types( CChar
                      )

data VkApplicationInfo = VkApplicationInfo { sType :: VkStructureType
, pNext :: Ptr Void
, pApplicationName :: Ptr CChar
, applicationVersion :: Word32
, pEngineName :: Ptr CChar
, engineVersion :: Word32
, apiVersion :: Word32 }
