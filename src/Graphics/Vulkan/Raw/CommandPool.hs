{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Raw.CommandPool where

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


data VkCommandPoolCreateInfo =
  VkCommandPoolCreateInfo{ vkSType :: VkStructureType 
                         , vkPNext :: Ptr Void 
                         , vkFlags :: VkCommandPoolCreateFlags 
                         , vkQueueFamilyIndex :: Word32 
                         }
  deriving (Eq, Ord)

instance Storable VkCommandPoolCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkCommandPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueueFamilyIndex (poked :: VkCommandPoolCreateInfo))


-- ** vkDestroyCommandPool
foreign import ccall "vkDestroyCommandPool" vkDestroyCommandPool ::
  VkDevice -> VkCommandPool -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkResetCommandPool
foreign import ccall "vkResetCommandPool" vkResetCommandPool ::
  VkDevice -> VkCommandPool -> VkCommandPoolResetFlags -> IO VkResult

-- ** VkCommandPoolCreateFlags

newtype VkCommandPoolCreateFlagBits = VkCommandPoolCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkCommandPoolCreateFlagBits
type VkCommandPoolCreateFlags = VkCommandPoolCreateFlagBits

instance Show VkCommandPoolCreateFlagBits where
  showsPrec _ VK_COMMAND_POOL_CREATE_TRANSIENT_BIT = showString "VK_COMMAND_POOL_CREATE_TRANSIENT_BIT"
  showsPrec _ VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = showString "VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT"
  
  showsPrec p (VkCommandPoolCreateFlagBits x) = showParen (p >= 11) (showString "VkCommandPoolCreateFlagBits " . showsPrec 11 x)

instance Read VkCommandPoolCreateFlagBits where
  readPrec = parens ( choose [ ("VK_COMMAND_POOL_CREATE_TRANSIENT_BIT", pure VK_COMMAND_POOL_CREATE_TRANSIENT_BIT)
                             , ("VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT", pure VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandPoolCreateFlagBits")
                        v <- step readPrec
                        pure (VkCommandPoolCreateFlagBits v)
                        )
                    )

-- | Command buffers have a short lifetime
pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT = VkCommandPoolCreateFlagBits 0x1
-- | Command buffers may release their memory individually
pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = VkCommandPoolCreateFlagBits 0x2


-- ** vkCreateCommandPool
foreign import ccall "vkCreateCommandPool" vkCreateCommandPool ::
  VkDevice ->
  Ptr VkCommandPoolCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkCommandPool -> IO VkResult

-- ** VkCommandPoolResetFlags

newtype VkCommandPoolResetFlagBits = VkCommandPoolResetFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkCommandPoolResetFlagBits
type VkCommandPoolResetFlags = VkCommandPoolResetFlagBits

instance Show VkCommandPoolResetFlagBits where
  showsPrec _ VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = showString "VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT"
  
  showsPrec p (VkCommandPoolResetFlagBits x) = showParen (p >= 11) (showString "VkCommandPoolResetFlagBits " . showsPrec 11 x)

instance Read VkCommandPoolResetFlagBits where
  readPrec = parens ( choose [ ("VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT", pure VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandPoolResetFlagBits")
                        v <- step readPrec
                        pure (VkCommandPoolResetFlagBits v)
                        )
                    )

-- | Release resources owned by the pool
pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = VkCommandPoolResetFlagBits 0x1


newtype VkCommandPool = VkCommandPool Word64
  deriving (Eq, Ord, Storable)

