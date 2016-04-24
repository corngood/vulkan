{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.CommandBuffer where

import Graphics.Vulkan.Device( Device(..)
                             )
import Graphics.Vulkan.Pass( RenderPass(..)
                           , Framebuffer(..)
                           )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , Ptr
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.CommandPool( CommandPool(..)
                                  )
import Data.Void( Void(..)
                )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Query( QueryPipelineStatisticFlags(..)
                            , QueryControlFlags(..)
                            )
import Graphics.Vulkan.Core( VkFlags(..)
                           , StructureType(..)
                           , VkBool32(..)
                           , Result(..)
                           )

-- ** CommandBufferLevel

newtype CommandBufferLevel = CommandBufferLevel Int32
  deriving (Eq, Storable)

instance Show CommandBufferLevel where
  showsPrec _ VK_COMMAND_BUFFER_LEVEL_PRIMARY = showString "VK_COMMAND_BUFFER_LEVEL_PRIMARY"
  showsPrec _ VK_COMMAND_BUFFER_LEVEL_SECONDARY = showString "VK_COMMAND_BUFFER_LEVEL_SECONDARY"
  showsPrec p (CommandBufferLevel x) = showParen (p >= 11) (showString "CommandBufferLevel " . showsPrec 11 x)

instance Read CommandBufferLevel where
  readPrec = parens ( choose [ ("VK_COMMAND_BUFFER_LEVEL_PRIMARY", pure VK_COMMAND_BUFFER_LEVEL_PRIMARY)
                             , ("VK_COMMAND_BUFFER_LEVEL_SECONDARY", pure VK_COMMAND_BUFFER_LEVEL_SECONDARY)
                             ] +++
                      prec 10 (do
                        expectP (Ident "CommandBufferLevel")
                        v <- step readPrec
                        pure (CommandBufferLevel v)
                        )
                    )


pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY = CommandBufferLevel 0

pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY = CommandBufferLevel 1

-- ** vkAllocateCommandBuffers
foreign import ccall "vkAllocateCommandBuffers" vkAllocateCommandBuffers ::
  Device ->
  Ptr CommandBufferAllocateInfo -> Ptr CommandBuffer -> IO Result

-- ** vkResetCommandBuffer
foreign import ccall "vkResetCommandBuffer" vkResetCommandBuffer ::
  CommandBuffer -> CommandBufferResetFlags -> IO Result

-- ** vkFreeCommandBuffers
foreign import ccall "vkFreeCommandBuffers" vkFreeCommandBuffers ::
  Device -> CommandPool -> Word32 -> Ptr CommandBuffer -> IO ()

-- ** VkCommandBufferUsageFlags

newtype CommandBufferUsageFlags = CommandBufferUsageFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show CommandBufferUsageFlags where
  showsPrec _ VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = showString "VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT"
  showsPrec _ VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = showString "VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT"
  showsPrec _ VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = showString "VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT"
  
  showsPrec p (CommandBufferUsageFlags x) = showParen (p >= 11) (showString "CommandBufferUsageFlags " . showsPrec 11 x)

instance Read CommandBufferUsageFlags where
  readPrec = parens ( choose [ ("VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT", pure VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
                             , ("VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT", pure VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT)
                             , ("VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT", pure VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "CommandBufferUsageFlags")
                        v <- step readPrec
                        pure (CommandBufferUsageFlags v)
                        )
                    )


pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = CommandBufferUsageFlags 0x1

pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = CommandBufferUsageFlags 0x2
-- | Command buffer may be submitted/executed more than once simultaneously
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = CommandBufferUsageFlags 0x4



data CommandBufferBeginInfo =
  CommandBufferBeginInfo{ sType :: StructureType 
                        , pNext :: Ptr Void 
                        , flags :: CommandBufferUsageFlags 
                        , pInheritanceInfo :: Ptr CommandBufferInheritanceInfo 
                        }
  deriving (Eq)

instance Storable CommandBufferBeginInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = CommandBufferBeginInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: CommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: CommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: CommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 24) (pInheritanceInfo (poked :: CommandBufferBeginInfo))



data CommandBufferInheritanceInfo =
  CommandBufferInheritanceInfo{ sType :: StructureType 
                              , pNext :: Ptr Void 
                              , renderPass :: RenderPass 
                              , subpass :: Word32 
                              , framebuffer :: Framebuffer 
                              , occlusionQueryEnable :: VkBool32 
                              , queryFlags :: QueryControlFlags 
                              , pipelineStatistics :: QueryPipelineStatisticFlags 
                              }
  deriving (Eq)

instance Storable CommandBufferInheritanceInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = CommandBufferInheritanceInfo <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
                                          <*> peek (ptr `plusPtr` 24)
                                          <*> peek (ptr `plusPtr` 32)
                                          <*> peek (ptr `plusPtr` 40)
                                          <*> peek (ptr `plusPtr` 44)
                                          <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: CommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: CommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 16) (renderPass (poked :: CommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 24) (subpass (poked :: CommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 32) (framebuffer (poked :: CommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 40) (occlusionQueryEnable (poked :: CommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 44) (queryFlags (poked :: CommandBufferInheritanceInfo))
                *> poke (ptr `plusPtr` 48) (pipelineStatistics (poked :: CommandBufferInheritanceInfo))


data VkCommandBuffer_T
type CommandBuffer = Ptr VkCommandBuffer_T

-- ** VkCommandBufferResetFlags

newtype CommandBufferResetFlags = CommandBufferResetFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show CommandBufferResetFlags where
  showsPrec _ VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = showString "VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT"
  
  showsPrec p (CommandBufferResetFlags x) = showParen (p >= 11) (showString "CommandBufferResetFlags " . showsPrec 11 x)

instance Read CommandBufferResetFlags where
  readPrec = parens ( choose [ ("VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT", pure VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "CommandBufferResetFlags")
                        v <- step readPrec
                        pure (CommandBufferResetFlags v)
                        )
                    )

-- | Release resources owned by the buffer
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = CommandBufferResetFlags 0x1


-- ** vkEndCommandBuffer
foreign import ccall "vkEndCommandBuffer" vkEndCommandBuffer ::
  CommandBuffer -> IO Result

-- ** vkBeginCommandBuffer
foreign import ccall "vkBeginCommandBuffer" vkBeginCommandBuffer ::
  CommandBuffer -> Ptr CommandBufferBeginInfo -> IO Result


data CommandBufferAllocateInfo =
  CommandBufferAllocateInfo{ sType :: StructureType 
                           , pNext :: Ptr Void 
                           , commandPool :: CommandPool 
                           , level :: CommandBufferLevel 
                           , commandBufferCount :: Word32 
                           }
  deriving (Eq)

instance Storable CommandBufferAllocateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = CommandBufferAllocateInfo <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: CommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: CommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 16) (commandPool (poked :: CommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 24) (level (poked :: CommandBufferAllocateInfo))
                *> poke (ptr `plusPtr` 28) (commandBufferCount (poked :: CommandBufferAllocateInfo))


