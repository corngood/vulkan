{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Raw.Fence where

import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
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
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Raw.Core( VkResult(..)
                               , VkBool32(..)
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


data VkFenceCreateInfo =
  VkFenceCreateInfo{ vkSType :: VkStructureType
                   , vkPNext :: Ptr Void
                   , vkFlags :: VkFenceCreateFlags
                   }
  deriving (Eq, Ord, Show)

instance Storable VkFenceCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkFenceCreateInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFenceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFenceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkFenceCreateInfo))


-- ** vkResetFences
foreign import ccall "vkResetFences" vkResetFences ::
  VkDevice -> Word32 -> Ptr VkFence -> IO VkResult

-- ** vkDestroyFence
foreign import ccall "vkDestroyFence" vkDestroyFence ::
  VkDevice -> VkFence -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkWaitForFences
foreign import ccall "vkWaitForFences" vkWaitForFences ::
  VkDevice ->
  Word32 -> Ptr VkFence -> VkBool32 -> Word64 -> IO VkResult

-- ** vkGetFenceStatus
foreign import ccall "vkGetFenceStatus" vkGetFenceStatus ::
  VkDevice -> VkFence -> IO VkResult

-- ** VkFenceCreateFlags

newtype VkFenceCreateFlagBits = VkFenceCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkFenceCreateFlagBits
type VkFenceCreateFlags = VkFenceCreateFlagBits

instance Show VkFenceCreateFlagBits where
  showsPrec _ VK_FENCE_CREATE_SIGNALED_BIT = showString "VK_FENCE_CREATE_SIGNALED_BIT"
  
  showsPrec p (VkFenceCreateFlagBits x) = showParen (p >= 11) (showString "VkFenceCreateFlagBits " . showsPrec 11 x)

instance Read VkFenceCreateFlagBits where
  readPrec = parens ( choose [ ("VK_FENCE_CREATE_SIGNALED_BIT", pure VK_FENCE_CREATE_SIGNALED_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFenceCreateFlagBits")
                        v <- step readPrec
                        pure (VkFenceCreateFlagBits v)
                        )
                    )


pattern VK_FENCE_CREATE_SIGNALED_BIT = VkFenceCreateFlagBits 0x1


-- ** vkCreateFence
foreign import ccall "vkCreateFence" vkCreateFence ::
  VkDevice ->
  Ptr VkFenceCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkFence -> IO VkResult

newtype VkFence = VkFence Word64
  deriving (Eq, Ord, Storable, Show)

