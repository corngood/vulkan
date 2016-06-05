{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Vulkan where

import Foreign.Ptr( Ptr(..)
                  , nullPtr
                  )
import Data.Word( Word32(..)
                )
import qualified Graphics.Vulkan.Raw.Core as Vk ( pattern StructureTypeApplicationInfo
                                                , StructureType
                                                )
import Foreign.C.String( withCString
                       , CString
                       )
import Data.Void( Void
                )
import qualified Graphics.Vulkan.Raw.Version as Vk ( makeVersion
                                                   )
import Foreign.C.Types( CChar(..)
                      )
import qualified Graphics.Vulkan.Raw.DeviceInitialization as Vk ( ApplicationInfo(..)
                                                                )
import Foreign.Marshal( with
                      )


data Version = Version Int Int Int

class WithVk a b | a -> b where
  withVk :: a -> (b -> IO c) -> IO c

instance WithVk String CString where
  withVk = withCString

instance WithVk Version Word32 where
  withVk (Version a b c) f = f v
    where v = Vk.makeVersion (fromIntegral a) (fromIntegral b) (fromIntegral c)

instance WithVk Vk.StructureType Vk.StructureType where
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

instance WithVk ApplicationInfo Vk.ApplicationInfo where
  withVk i f = wrapValue Vk.StructureTypeApplicationInfo (wrapValue (nullPtr :: Ptr Void) (wrapValue (pApplicationName i) (wrapValue (applicationVersion i) (wrapValue (pEngineName i) (wrapValue (engineVersion i) (wrapValue (apiVersion i) (f))))))) Vk.ApplicationInfo
