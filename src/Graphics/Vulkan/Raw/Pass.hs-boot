{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Raw.Pass where

import Foreign.Storable( Storable(..)
                       )
import Data.Word( Word64(..)
                )

newtype RenderPass = RenderPass Word64
  
instance Eq RenderPass
instance Ord RenderPass
instance Storable RenderPass

