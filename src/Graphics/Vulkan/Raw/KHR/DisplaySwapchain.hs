{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.Raw.KHR.DisplaySwapchain where

import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.Raw.KHR.Swapchain( VkSwapchainKHR(..)
                                        , VkSwapchainCreateInfoKHR(..)
                                        , VkSwapchainCreateFlagsKHR(..)
                                        )
import Graphics.Vulkan.Raw.Device( VkDevice(..)
                                 )
import Data.Int( Int32
               )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Raw.Image( VkImageUsageFlags(..)
                                , VkImageUsageFlagBits(..)
                                )
import Data.Void( Void
                )
import Graphics.Vulkan.Raw.KHR.Surface( VkColorSpaceKHR(..)
                                      , VkSurfaceTransformFlagBitsKHR(..)
                                      , VkPresentModeKHR(..)
                                      , VkCompositeAlphaFlagBitsKHR(..)
                                      , VkSurfaceKHR(..)
                                      )
import Graphics.Vulkan.Raw.Core( VkResult(..)
                               , VkBool32(..)
                               , VkExtent2D(..)
                               , VkFlags(..)
                               , VkFormat(..)
                               , VkOffset2D(..)
                               , VkRect2D(..)
                               , VkStructureType(..)
                               , VkSharingMode(..)
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


data VkDisplayPresentInfoKHR =
  VkDisplayPresentInfoKHR{ vkSType :: VkStructureType
                         , vkPNext :: Ptr Void
                         , vkSrcRect :: VkRect2D
                         , vkDstRect :: VkRect2D
                         , vkPersistent :: VkBool32
                         }
  deriving (Eq, Ord, Show)

instance Storable VkDisplayPresentInfoKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkDisplayPresentInfoKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 32)
                                     <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSrcRect (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkDstRect (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkPersistent (poked :: VkDisplayPresentInfoKHR))


-- ** vkCreateSharedSwapchainsKHR
foreign import ccall "vkCreateSharedSwapchainsKHR" vkCreateSharedSwapchainsKHR ::
  VkDevice ->
  Word32 ->
    Ptr VkSwapchainCreateInfoKHR ->
      Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR -> IO VkResult

