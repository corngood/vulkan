{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Raw.Event where

import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
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
                               , VkFlags(..)
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

-- ** vkDestroyEvent
foreign import ccall "vkDestroyEvent" vkDestroyEvent ::
  VkDevice -> VkEvent -> Ptr VkAllocationCallbacks -> IO ()


data VkEventCreateInfo =
  VkEventCreateInfo{ vkSType :: VkStructureType
                   , vkPNext :: Ptr Void
                   , vkFlags :: VkEventCreateFlags
                   }
  deriving (Eq, Ord, Show)

instance Storable VkEventCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkEventCreateInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkEventCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkEventCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkEventCreateInfo))


-- ** vkSetEvent
foreign import ccall "vkSetEvent" vkSetEvent ::
  VkDevice -> VkEvent -> IO VkResult

-- ** vkGetEventStatus
foreign import ccall "vkGetEventStatus" vkGetEventStatus ::
  VkDevice -> VkEvent -> IO VkResult

-- ** vkResetEvent
foreign import ccall "vkResetEvent" vkResetEvent ::
  VkDevice -> VkEvent -> IO VkResult

-- ** vkCreateEvent
foreign import ccall "vkCreateEvent" vkCreateEvent ::
  VkDevice ->
  Ptr VkEventCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkEvent -> IO VkResult

newtype VkEvent = VkEvent Word64
  deriving (Eq, Ord, Storable, Show)

-- ** VkEventCreateFlags
-- | Opaque flag
newtype VkEventCreateFlags = VkEventCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)

