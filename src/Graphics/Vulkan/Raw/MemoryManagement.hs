{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.Raw.MemoryManagement where

import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Raw.Image( Image(..)
                                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Data.Word( Word32(..)
                )
import Graphics.Vulkan.Raw.Buffer( Buffer(..)
                                 )
import Graphics.Vulkan.Raw.Device( Device(..)
                                 )
import Graphics.Vulkan.Raw.Core( Result(..)
                               , DeviceSize(..)
                               )
import Graphics.Vulkan.Raw.Memory( DeviceMemory(..)
                                 )

-- ** getImageMemoryRequirements
foreign import ccall "vkGetImageMemoryRequirements" getImageMemoryRequirements ::
  Device -> Image -> Ptr MemoryRequirements -> IO ()


data MemoryRequirements =
  MemoryRequirements{ size :: DeviceSize 
                    , _alignment :: DeviceSize 
                    , memoryTypeBits :: Word32 
                    }
  deriving (Eq, Ord)

instance Storable MemoryRequirements where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = MemoryRequirements <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (size (poked :: MemoryRequirements))
                *> poke (ptr `plusPtr` 8) (_alignment (poked :: MemoryRequirements))
                *> poke (ptr `plusPtr` 16) (memoryTypeBits (poked :: MemoryRequirements))


-- ** getBufferMemoryRequirements
foreign import ccall "vkGetBufferMemoryRequirements" getBufferMemoryRequirements ::
  Device -> Buffer -> Ptr MemoryRequirements -> IO ()

-- ** bindBufferMemory
foreign import ccall "vkBindBufferMemory" bindBufferMemory ::
  Device -> Buffer -> DeviceMemory -> DeviceSize -> IO Result

-- ** bindImageMemory
foreign import ccall "vkBindImageMemory" bindImageMemory ::
  Device -> Image -> DeviceMemory -> DeviceSize -> IO Result

