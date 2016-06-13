{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.Raw.NV.GlslShader where

import Graphics.Vulkan.Raw.Core( VkResult(..)
                               )

pattern VK_ERROR_INVALID_SHADER_NV = VkResult (-1000012000)
