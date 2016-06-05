{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.CommandBuffer where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Graphics.Vulkan.Pass( VkFramebuffer(..)
                           , VkRenderPass(..)
                           )
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
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.CommandPool( VkCommandPool(..)
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
import Graphics.Vulkan.Query( VkQueryPipelineStatisticFlags(..)
                            , VkQueryControlFlagBits(..)
                            , VkQueryControlFlags(..)
                            , VkQueryPipelineStatisticFlagBits(..)
                            )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkBool32(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )

-- ** VkCommandBufferLevel

newtype VkCommandBufferLevel = VkCommandBufferLevel Int32
  deriving (Eq, Ord, Storable)

instance Show VkCommandBufferLevel where
  showsPrec _ VK_COMMAND_BUFFER_LEVEL_PRIMARY = showString "VK_COMMAND_BUFFER_LEVEL_PRIMARY"
  showsPrec _ VK_COMMAND_BUFFER_LEVEL_SECONDARY = showString "VK_COMMAND_BUFFER_LEVEL_SECONDARY"
  showsPrec p (VkCommandBufferLevel x) = showParen (p >= 11) (showString "VkCommandBufferLevel " . showsPrec 11 x)

instance Read VkCommandBufferLevel where
  readPrec = parens ( choose [ ("VK_COMMAND_BUFFER_LEVEL_PRIMARY", pure VK_COMMAND_BUFFER_LEVEL_PRIMARY)
                             , ("VK_COMMAND_BUFFER_LEVEL_SECONDARY", pure VK_COMMAND_BUFFER_LEVEL_SECONDARY)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandBufferLevel")
                        v <- step readPrec
                        pure (VkCommandBufferLevel v)
                        )
                    )


pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY = VkCommandBufferLevel 0

pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY = VkCommandBufferLevel 1

-- ** vkAllocateCommandBuffers
foreign import ccall "vkAllocateCommandBuffers" vkAllocateCommandBuffers ::
  VkDevice ->
  Ptr VkCommandBufferAllocateInfo ->
    Ptr VkCommandBuffer -> IO VkResult

-- ** vkResetCommandBuffer
foreign import ccall "vkResetCommandBuffer" vkResetCommandBuffer ::
  VkCommandBuffer -> VkCommandBufferResetFlags -> IO VkResult

-- ** vkFreeCommandBuffers
foreign import ccall "vkFreeCommandBuffers" vkFreeCommandBuffers ::
  VkDevice -> VkCommandPool -> Word32 -> Ptr VkCommandBuffer -> IO ()

-- ** VkCommandBufferUsageFlags

newtype VkCommandBufferUsageFlagBits = VkCommandBufferUsageFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkCommandBufferUsageFlagBits
type VkCommandBufferUsageFlags = VkCommandBufferUsageFlagBits

instance Show VkCommandBufferUsageFlagBits where
  showsPrec _ VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = showString "VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT"
  showsPrec _ VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = showString "VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT"
  showsPrec _ VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = showString "VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT"
  
  showsPrec p (VkCommandBufferUsageFlagBits x) = showParen (p >= 11) (showString "VkCommandBufferUsageFlagBits " . showsPrec 11 x)

instance Read VkCommandBufferUsageFlagBits where
  readPrec = parens ( choose [ ("VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT", pure VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
                             , ("VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT", pure VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT)
                             , ("VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT", pure VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandBufferUsageFlagBits")
                        v <- step readPrec
                        pure (VkCommandBufferUsageFlagBits v)
                        )
                    )


pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = VkCommandBufferUsageFlagBits 0x1

pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = VkCommandBufferUsageFlagBits 0x2
-- | Command buffer may be submitted/executed more than once simultaneously
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = VkCommandBufferUsageFlagBits 0x4



data VkCommandBufferBeginInfo =
  VkCommandBufferBeginInfo{ vkSType :: VkStructureType 
                          , vkPNext :: Ptr Void 
                          , vkFlags :: VkCommandBufferUsageFlags 
                          , vkPInheritanceInfo :: Ptr VkCommandBufferInheritanceInfo 
                          }
  deriving (Eq, Ord)

instance Storable VkCommandBufferBeginInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkCommandBufferBeginInfo <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 24) (vkPInheritanceInfo (poked :: VkCommandBufferBeginInfo))



data VkCommandBufferInheritanceInfo =
  VkCommandBufferInheritanceInfo{ vkSType :: VkStructureType 
                                , vkPNext :: Ptr Void 
                                , vkRenderPass :: VkRenderPass 
                                , vkSubpass :: Word32 
                                , vkFramebuffer :: VkFramebuffer 
                                , vkOcclusionQueryEnable :: VkBool32 
                                , vkQueryFlags :: VkQueryControlFlags 
                                , vkPipelineStatistics :: VkQueryPipelineStatisticFlags 
                                }
  deriving (Eq, Ord)

instance Storable VkCommandBufferInheritanceInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkCommandBufferInheritanceInfo <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
                                            <*> peek (ptr `plusPtr` 24)
                                            <*> peek (ptr `plusPtr` 32)
                                            <*> peek (ptr `plusPtr` 40)
                                            <*> peek (ptr `plusPtr` 44)
                                            <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 16) (vkRenderPass (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 24) (vkSubpass (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 32) (vkFramebuffer (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 40) (vkOcclusionQueryEnable (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 44) (vkQueryFlags (poked :: VkCommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 48) (vkPipelineStatistics (poked :: VkCommandBufferInheritanceInfo))


data VkCommandBuffer_T
type VkCommandBuffer = Ptr VkCommandBuffer_T

-- ** VkCommandBufferResetFlags

newtype VkCommandBufferResetFlagBits = VkCommandBufferResetFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkCommandBufferResetFlagBits
type VkCommandBufferResetFlags = VkCommandBufferResetFlagBits

instance Show VkCommandBufferResetFlagBits where
  showsPrec _ VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = showString "VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT"
  
  showsPrec p (VkCommandBufferResetFlagBits x) = showParen (p >= 11) (showString "VkCommandBufferResetFlagBits " . showsPrec 11 x)

instance Read VkCommandBufferResetFlagBits where
  readPrec = parens ( choose [ ("VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT", pure VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandBufferResetFlagBits")
                        v <- step readPrec
                        pure (VkCommandBufferResetFlagBits v)
                        )
                    )

-- | Release resources owned by the buffer
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = VkCommandBufferResetFlagBits 0x1


-- ** vkEndCommandBuffer
foreign import ccall "vkEndCommandBuffer" vkEndCommandBuffer ::
  VkCommandBuffer -> IO VkResult

-- ** vkBeginCommandBuffer
foreign import ccall "vkBeginCommandBuffer" vkBeginCommandBuffer ::
  VkCommandBuffer -> Ptr VkCommandBufferBeginInfo -> IO VkResult


data VkCommandBufferAllocateInfo =
  VkCommandBufferAllocateInfo{ vkSType :: VkStructureType 
                             , vkPNext :: Ptr Void 
                             , vkCommandPool :: VkCommandPool 
                             , vkLevel :: VkCommandBufferLevel 
                             , vkCommandBufferCount :: Word32 
                             }
  deriving (Eq, Ord)

instance Storable VkCommandBufferAllocateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkCommandBufferAllocateInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkCommandPool (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 24) (vkLevel (poked :: VkCommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 28) (vkCommandBufferCount (poked :: VkCommandBufferAllocateInfo))


