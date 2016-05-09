{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.Raw.EXT.DebugMarker where

import Data.Vector.Storable.Sized( Vector
                                 )
import Graphics.Vulkan.Raw.CommandBuffer( VkCommandBuffer(..)
                                        )
import Graphics.Vulkan.Raw.EXT.DebugReport( VkDebugReportObjectTypeEXT(..)
                                          )
import System.IO.Unsafe( unsafePerformIO
                       )
import Data.Word( Word64
                )
import Foreign.Ptr( Ptr
                  , FunPtr
                  , plusPtr
                  , castFunPtr
                  )
import Graphics.Vulkan.Raw.Device( VkDevice
                                 , VkDevice(..)
                                 )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Raw.DeviceInitialization( vkGetDeviceProcAddr
                                               )
import Graphics.Vulkan.Raw.Core( VkResult(..)
                               , VkStructureType(..)
                               )
import Foreign.C.String( withCString
                       )
import Foreign.C.Types( CSize
                      , CFloat
                      , CFloat(..)
                      , CChar
                      , CSize(..)
                      )


data VkDebugMarkerObjectNameInfoEXT =
  VkDebugMarkerObjectNameInfoEXT{ vkSType :: VkStructureType
                                , vkPNext :: Ptr Void
                                , vkObjectType :: VkDebugReportObjectTypeEXT
                                , vkObject :: Word64
                                , vkPObjectName :: Ptr CChar
                                }
  deriving (Eq, Ord, Show)

instance Storable VkDebugMarkerObjectNameInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDebugMarkerObjectNameInfoEXT <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
                                            <*> peek (ptr `plusPtr` 24)
                                            <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugMarkerObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugMarkerObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkObjectType (poked :: VkDebugMarkerObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkObject (poked :: VkDebugMarkerObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPObjectName (poked :: VkDebugMarkerObjectNameInfoEXT))



data VkDebugMarkerMarkerInfoEXT =
  VkDebugMarkerMarkerInfoEXT{ vkSType :: VkStructureType
                            , vkPNext :: Ptr Void
                            , vkPMarkerName :: Ptr CChar
                            , vkColor :: Vector 4 CFloat
                            }
  deriving (Eq, Ord, Show)

instance Storable VkDebugMarkerMarkerInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDebugMarkerMarkerInfoEXT <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugMarkerMarkerInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugMarkerMarkerInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkPMarkerName (poked :: VkDebugMarkerMarkerInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkColor (poked :: VkDebugMarkerMarkerInfoEXT))


-- ** vkCmdDebugMarkerInsertEXT
foreign import ccall "dynamic" mkvkCmdDebugMarkerInsertEXT :: FunPtr (VkCommandBuffer -> Ptr VkDebugMarkerMarkerInfoEXT -> IO ()) -> (VkCommandBuffer -> Ptr VkDebugMarkerMarkerInfoEXT -> IO ())
vkCmdDebugMarkerInsertEXT :: VkDevice ->
  VkCommandBuffer -> Ptr VkDebugMarkerMarkerInfoEXT -> IO ()
vkCmdDebugMarkerInsertEXT d = (mkvkCmdDebugMarkerInsertEXT $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkCmdDebugMarkerInsertEXT" $ vkGetDeviceProcAddr d

-- ** vkCmdDebugMarkerBeginEXT
foreign import ccall "dynamic" mkvkCmdDebugMarkerBeginEXT :: FunPtr (VkCommandBuffer -> Ptr VkDebugMarkerMarkerInfoEXT -> IO ()) -> (VkCommandBuffer -> Ptr VkDebugMarkerMarkerInfoEXT -> IO ())
vkCmdDebugMarkerBeginEXT :: VkDevice ->
  VkCommandBuffer -> Ptr VkDebugMarkerMarkerInfoEXT -> IO ()
vkCmdDebugMarkerBeginEXT d = (mkvkCmdDebugMarkerBeginEXT $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkCmdDebugMarkerBeginEXT" $ vkGetDeviceProcAddr d

-- ** vkDebugMarkerSetObjectTagEXT
foreign import ccall "dynamic" mkvkDebugMarkerSetObjectTagEXT :: FunPtr (VkDevice -> Ptr VkDebugMarkerObjectTagInfoEXT -> IO VkResult) -> (VkDevice -> Ptr VkDebugMarkerObjectTagInfoEXT -> IO VkResult)
vkDebugMarkerSetObjectTagEXT :: VkDevice -> Ptr VkDebugMarkerObjectTagInfoEXT -> IO VkResult
vkDebugMarkerSetObjectTagEXT d = (mkvkDebugMarkerSetObjectTagEXT $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkDebugMarkerSetObjectTagEXT" $ vkGetDeviceProcAddr d

-- ** vkCmdDebugMarkerEndEXT
foreign import ccall "dynamic" mkvkCmdDebugMarkerEndEXT :: FunPtr (VkCommandBuffer -> IO ()) -> (VkCommandBuffer -> IO ())
vkCmdDebugMarkerEndEXT :: VkDevice -> VkCommandBuffer -> IO ()
vkCmdDebugMarkerEndEXT d = (mkvkCmdDebugMarkerEndEXT $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkCmdDebugMarkerEndEXT" $ vkGetDeviceProcAddr d


data VkDebugMarkerObjectTagInfoEXT =
  VkDebugMarkerObjectTagInfoEXT{ vkSType :: VkStructureType
                               , vkPNext :: Ptr Void
                               , vkObjectType :: VkDebugReportObjectTypeEXT
                               , vkObject :: Word64
                               , vkTagName :: Word64
                               , vkTagSize :: CSize
                               , vkPTag :: Ptr Void
                               }
  deriving (Eq, Ord, Show)

instance Storable VkDebugMarkerObjectTagInfoEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkDebugMarkerObjectTagInfoEXT <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
                                           <*> peek (ptr `plusPtr` 32)
                                           <*> peek (ptr `plusPtr` 40)
                                           <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugMarkerObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugMarkerObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkObjectType (poked :: VkDebugMarkerObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkObject (poked :: VkDebugMarkerObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkTagName (poked :: VkDebugMarkerObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 40) (vkTagSize (poked :: VkDebugMarkerObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 48) (vkPTag (poked :: VkDebugMarkerObjectTagInfoEXT))


-- ** vkDebugMarkerSetObjectNameEXT
foreign import ccall "dynamic" mkvkDebugMarkerSetObjectNameEXT :: FunPtr (VkDevice -> Ptr VkDebugMarkerObjectNameInfoEXT -> IO VkResult) -> (VkDevice -> Ptr VkDebugMarkerObjectNameInfoEXT -> IO VkResult)
vkDebugMarkerSetObjectNameEXT :: VkDevice -> Ptr VkDebugMarkerObjectNameInfoEXT -> IO VkResult
vkDebugMarkerSetObjectNameEXT d = (mkvkDebugMarkerSetObjectNameEXT $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkDebugMarkerSetObjectNameEXT" $ vkGetDeviceProcAddr d

