{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan where

import Foreign.Ptr( Ptr(..)
                  , nullPtr
                  )
import Data.Word( Word32(..)
                )
import qualified Graphics.Vulkan.Raw.Core as Vk ( pattern StructureTypeApplicationInfo
                                                )
import Foreign.C.Types( CChar(..)
                      )
import qualified Graphics.Vulkan.Raw.DeviceInitialization as Vk ( ApplicationInfo(..)
                                                                )
import Foreign.Marshal( with
                      )


data ApplicationInfo = ApplicationInfo {
pApplicationName :: Ptr CChar
, applicationVersion :: Word32
, pEngineName :: Ptr CChar
, engineVersion :: Word32
, apiVersion :: Word32
}

wrap :: ApplicationInfo -> (Ptr Vk.ApplicationInfo -> IO a) -> IO a
wrap i f = with (Vk.ApplicationInfo Vk.StructureTypeApplicationInfo nullPtr (pApplicationName i) (applicationVersion i) (pEngineName i) (engineVersion i) (apiVersion i)) f
