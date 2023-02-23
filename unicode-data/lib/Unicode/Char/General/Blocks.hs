{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Unicode.Char.General.Blocks
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode [blocks](https://www.unicode.org/glossary/#block) related functions.
--
-- @since 0.3.1

module Unicode.Char.General.Blocks
    ( -- * Blocks
      B.Block(..)
    , block
      -- * Blocks definitions
    , B.BlockDefinition()
    , pattern BlockDefinition
    , blockRange
    , blockName
    , B.blockDefinition
    )

where

import Foreign.C.String (CString)
import GHC.Exts (Ptr(..))

import Unicode.Internal.Bits (unpackCString#)
import qualified Unicode.Internal.Char.Blocks as B

-- | Character [block](https://www.unicode.org/glossary/#block), if defined.
--
-- @since 0.3.1
{-# INLINE block #-}
block :: Char -> Maybe B.Block
block = fmap toEnum . B.block

-- | Definition of a Unicode block: range and name.
--
-- @since 0.4.1
pattern BlockDefinition :: (Int, Int) -> String -> B.BlockDefinition
pattern BlockDefinition{blockRange, blockName} <-
    B.BlockDefinition blockRange (unpack -> blockName)
{-# COMPLETE BlockDefinition :: B.BlockDefinition #-}

-- | Range of characters of a block.
blockRange :: B.BlockDefinition -> (Int, Int)

-- | Name of a block.
blockName :: B.BlockDefinition -> String

-- Note: names are ASCII. See Unicode Standard 15.0.0, section 3.4.
{-# INLINE unpack #-}
unpack :: CString -> String
unpack (Ptr addr#) = unpackCString# addr#
