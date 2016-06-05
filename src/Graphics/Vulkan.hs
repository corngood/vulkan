{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Vulkan where

import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , nullPtr
                  )
import Graphics.Vulkan.Raw.Version( vkMakeVersion
                                  )
import Foreign.Marshal( with
                      )
import Data.Void( Void
                )
import Graphics.Vulkan.Raw.DeviceInitialization( VkApplicationInfo(..)
                                               )
import Graphics.Vulkan.Raw.Core( VkStructureType
                               , pattern VK_STRUCTURE_TYPE_APPLICATION_INFO
                               )
import Foreign.C.String( withCString
                       , CString
                       )
import Foreign.C.Types( CChar
                      )


data Version = Version Int Int Int

class WithVk a b | a -> b where
  withVk :: a -> (b -> IO c) -> IO c

instance WithVk String CString where
  withVk = withCString

instance WithVk Version Word32 where
  withVk (Version a b c) f = f v
    where v = vkMakeVersion (fromIntegral a) (fromIntegral b) (fromIntegral c)

instance WithVk VkStructureType VkStructureType where
  withVk a f = f a

instance WithVk (Ptr Void) (Ptr Void) where
  withVk a f = f a

wrapValue :: (WithVk a b) => a -> (c -> IO d) -> (b -> c) -> IO d
wrapValue a g f = undefined


data ApplicationInfo = ApplicationInfo {
pApplicationName :: String
, applicationVersion :: Version
, pEngineName :: String
, engineVersion :: Version
, apiVersion :: Version
}

instance WithVk ApplicationInfo VkApplicationInfo where
  withVk i f = wrapValue VK_STRUCTURE_TYPE_APPLICATION_INFO (wrapValue (nullPtr :: Ptr Void) (wrapValue (pApplicationName i) (wrapValue (applicationVersion i) (wrapValue (pEngineName i) (wrapValue (engineVersion i) (wrapValue (apiVersion i) (f))))))) VkApplicationInfo
