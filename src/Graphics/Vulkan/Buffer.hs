{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Buffer where

import Graphics.Vulkan.Device( Device(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64(..)
                , Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( AllocationCallbacks(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Core( VkStructureType(..)
                           , VkFlags(..)
                           , VkResult(..)
                           , VkDeviceSize(..)
                           , VkSharingMode(..)
                           )

-- ** vkCreateBuffer
foreign import ccall "vkCreateBuffer" vkCreateBuffer ::
  Device ->
  Ptr BufferCreateInfo ->
    Ptr AllocationCallbacks -> Ptr Buffer -> IO VkResult

-- ** VkBufferCreateFlags

newtype VkBufferCreateFlags = VkBufferCreateFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkBufferCreateFlags where
  showsPrec _ VK_BUFFER_CREATE_SPARSE_BINDING_BIT = showString "VK_BUFFER_CREATE_SPARSE_BINDING_BIT"
  showsPrec _ VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT = showString "VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT"
  showsPrec _ VK_BUFFER_CREATE_SPARSE_ALIASED_BIT = showString "VK_BUFFER_CREATE_SPARSE_ALIASED_BIT"
  
  showsPrec p (VkBufferCreateFlags x) = showParen (p >= 11) (showString "VkBufferCreateFlags " . showsPrec 11 x)

instance Read VkBufferCreateFlags where
  readPrec = parens ( choose [ ("VK_BUFFER_CREATE_SPARSE_BINDING_BIT", pure VK_BUFFER_CREATE_SPARSE_BINDING_BIT)
                             , ("VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT", pure VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT)
                             , ("VK_BUFFER_CREATE_SPARSE_ALIASED_BIT", pure VK_BUFFER_CREATE_SPARSE_ALIASED_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBufferCreateFlags")
                        v <- step readPrec
                        pure (VkBufferCreateFlags v)
                        )
                    )

-- | Buffer should support sparse backing
pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT = VkBufferCreateFlags 0x1
-- | Buffer should support sparse backing with partial residency
pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT = VkBufferCreateFlags 0x2
-- | Buffer should support constent data access to physical memory blocks mapped into multiple locations of sparse buffers
pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT = VkBufferCreateFlags 0x4


-- ** VkBufferUsageFlags

newtype VkBufferUsageFlags = VkBufferUsageFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkBufferUsageFlags where
  showsPrec _ VK_BUFFER_USAGE_TRANSFER_SRC_BIT = showString "VK_BUFFER_USAGE_TRANSFER_SRC_BIT"
  showsPrec _ VK_BUFFER_USAGE_TRANSFER_DST_BIT = showString "VK_BUFFER_USAGE_TRANSFER_DST_BIT"
  showsPrec _ VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = showString "VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = showString "VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT = showString "VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_STORAGE_BUFFER_BIT = showString "VK_BUFFER_USAGE_STORAGE_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_INDEX_BUFFER_BIT = showString "VK_BUFFER_USAGE_INDEX_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_VERTEX_BUFFER_BIT = showString "VK_BUFFER_USAGE_VERTEX_BUFFER_BIT"
  showsPrec _ VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT = showString "VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT"
  
  showsPrec p (VkBufferUsageFlags x) = showParen (p >= 11) (showString "VkBufferUsageFlags " . showsPrec 11 x)

instance Read VkBufferUsageFlags where
  readPrec = parens ( choose [ ("VK_BUFFER_USAGE_TRANSFER_SRC_BIT", pure VK_BUFFER_USAGE_TRANSFER_SRC_BIT)
                             , ("VK_BUFFER_USAGE_TRANSFER_DST_BIT", pure VK_BUFFER_USAGE_TRANSFER_DST_BIT)
                             , ("VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT", pure VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT", pure VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT", pure VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_STORAGE_BUFFER_BIT", pure VK_BUFFER_USAGE_STORAGE_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_INDEX_BUFFER_BIT", pure VK_BUFFER_USAGE_INDEX_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_VERTEX_BUFFER_BIT", pure VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)
                             , ("VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT", pure VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBufferUsageFlags")
                        v <- step readPrec
                        pure (VkBufferUsageFlags v)
                        )
                    )

-- | Can be used as a source of transfer operations
pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT = VkBufferUsageFlags 0x1
-- | Can be used as a destination of transfer operations
pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT = VkBufferUsageFlags 0x2
-- | Can be used as TBO
pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = VkBufferUsageFlags 0x4
-- | Can be used as IBO
pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = VkBufferUsageFlags 0x8
-- | Can be used as UBO
pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT = VkBufferUsageFlags 0x10
-- | Can be used as SSBO
pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT = VkBufferUsageFlags 0x20
-- | Can be used as source of fixed-function index fetch (index buffer)
pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT = VkBufferUsageFlags 0x40
-- | Can be used as source of fixed-function vertex fetch (VBO)
pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT = VkBufferUsageFlags 0x80
-- | Can be the source of indirect parameters (e.g. indirect buffer, parameter buffer)
pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT = VkBufferUsageFlags 0x100


-- ** vkDestroyBuffer
foreign import ccall "vkDestroyBuffer" vkDestroyBuffer ::
  Device -> Buffer -> Ptr AllocationCallbacks -> IO ()

newtype Buffer = Buffer Word64
  deriving (Eq, Storable)


data BufferCreateInfo =
  BufferCreateInfo{ sType :: VkStructureType 
                  , pNext :: Ptr Void 
                  , flags :: VkBufferCreateFlags 
                  , size :: VkDeviceSize 
                  , usage :: VkBufferUsageFlags 
                  , sharingMode :: VkSharingMode 
                  , queueFamilyIndexCount :: Word32 
                  , pQueueFamilyIndices :: Ptr Word32 
                  }
  deriving (Eq)

instance Storable BufferCreateInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = BufferCreateInfo <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 24)
                              <*> peek (ptr `plusPtr` 32)
                              <*> peek (ptr `plusPtr` 36)
                              <*> peek (ptr `plusPtr` 40)
                              <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: BufferCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: BufferCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: BufferCreateInfo))
                *> poke (ptr `plusPtr` 24) (size (poked :: BufferCreateInfo))
                *> poke (ptr `plusPtr` 32) (usage (poked :: BufferCreateInfo))
                *> poke (ptr `plusPtr` 36) (sharingMode (poked :: BufferCreateInfo))
                *> poke (ptr `plusPtr` 40) (queueFamilyIndexCount (poked :: BufferCreateInfo))
                *> poke (ptr `plusPtr` 48) (pQueueFamilyIndices (poked :: BufferCreateInfo))


