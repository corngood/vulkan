{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.Raw.ExtensionDiscovery where

import Foreign.Storable( Storable(..)
                       )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Data.Word( Word32(..)
                )
import Graphics.Vulkan.Raw.Constants( MaxExtensionNameSize
                                    )
import Graphics.Vulkan.Raw.Device( PhysicalDevice(..)
                                 )
import Foreign.C.Types( CChar(..)
                      )
import Data.Vector.Storable.Sized( Vector(..)
                                 )
import Graphics.Vulkan.Raw.Core( Result(..)
                               )


data ExtensionProperties =
  ExtensionProperties{ extensionName :: Vector MaxExtensionNameSize CChar 
                     , specVersion :: Word32 
                     }
  deriving (Eq, Ord)

instance Storable ExtensionProperties where
  sizeOf ~_ = 260
  alignment ~_ = 4
  peek ptr = ExtensionProperties <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 256)
  poke ptr poked = poke (ptr `plusPtr` 0) (extensionName (poked :: ExtensionProperties))
                *> poke (ptr `plusPtr` 256) (specVersion (poked :: ExtensionProperties))


-- ** enumerateInstanceExtensionProperties
foreign import ccall "vkEnumerateInstanceExtensionProperties" enumerateInstanceExtensionProperties ::
  Ptr CChar -> Ptr Word32 -> Ptr ExtensionProperties -> IO Result

-- ** enumerateDeviceExtensionProperties
foreign import ccall "vkEnumerateDeviceExtensionProperties" enumerateDeviceExtensionProperties ::
  PhysicalDevice ->
  Ptr CChar -> Ptr Word32 -> Ptr ExtensionProperties -> IO Result

