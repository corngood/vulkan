{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Raw.BufferView where

import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.Raw.Buffer( VkBuffer(..)
                                 )
import Graphics.Vulkan.Raw.Device( VkDevice(..)
                                 )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Raw.Core( VkResult(..)
                               , VkDeviceSize(..)
                               , VkFlags(..)
                               , VkFormat(..)
                               , VkStructureType(..)
                               )
import Graphics.Vulkan.Raw.Memory( VkInternalAllocationType(..)
                                 , PFN_vkAllocationFunction
                                 , PFN_vkReallocationFunction
                                 , PFN_vkInternalAllocationNotification
                                 , VkAllocationCallbacks(..)
                                 , VkSystemAllocationScope(..)
                                 , PFN_vkFreeFunction
                                 , PFN_vkInternalFreeNotification
                                 )
import Foreign.C.Types( CSize(..)
                      )

-- ** vkCreateBufferView
foreign import ccall "vkCreateBufferView" vkCreateBufferView ::
  VkDevice ->
  Ptr VkBufferViewCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkBufferView -> IO VkResult

newtype VkBufferView = VkBufferView Word64
  deriving (Eq, Ord, Storable, Show)


data VkBufferViewCreateInfo =
  VkBufferViewCreateInfo{ vkSType :: VkStructureType
                        , vkPNext :: Ptr Void
                        , vkFlags :: VkBufferViewCreateFlags
                        , vkBuffer :: VkBuffer
                        , vkFormat :: VkFormat
                        , vkOffset :: VkDeviceSize
                        , vkRange :: VkDeviceSize
                        }
  deriving (Eq, Ord, Show)

instance Storable VkBufferViewCreateInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkBufferViewCreateInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 40)
                                    <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkBuffer (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkFormat (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkOffset (poked :: VkBufferViewCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkRange (poked :: VkBufferViewCreateInfo))


-- ** VkBufferViewCreateFlags
-- | Opaque flag
newtype VkBufferViewCreateFlags = VkBufferViewCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)

-- ** vkDestroyBufferView
foreign import ccall "vkDestroyBufferView" vkDestroyBufferView ::
  VkDevice -> VkBufferView -> Ptr VkAllocationCallbacks -> IO ()

