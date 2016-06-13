{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.Raw.IMG.FilterCubic where

import Graphics.Vulkan.Raw.Sampler( VkFilter(..)
                                  )

pattern VK_FILTER_CUBIC_IMG = VkFilter 1000015000
