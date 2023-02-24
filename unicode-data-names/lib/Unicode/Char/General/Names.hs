-- |
-- Module      : Unicode.Char.General.Names
-- Copyright   : (c) 2022 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode character names and name aliases.
-- See Unicode standard 15.0.0, section 4.8.
--
-- @since 0.1.0

module Unicode.Char.General.Names
    ( -- * Name
      name
    , nameOrAlias
    , correctedName
      -- * Name Aliases
    , NameAliases.NameAliasType(..)
    -- , nameAliases
    , nameAliasesByType
    , nameAliasesWithTypes
    ) where

import Control.Applicative ((<|>))
import GHC.Exts
    ( Addr#, Char(..), Int#, Int(..)
    , indexCharOffAddr#, plusAddr#, dataToTag#, ord#, (+#), (<#), isTrue# )

import Unicode.Internal.Bits.Names (unpackCString#)
import qualified Unicode.Internal.Char.UnicodeData.DerivedName as DerivedName
import qualified Unicode.Internal.Char.UnicodeData.NameAliases as NameAliases

-- | Name of a character, if defined.
--
-- @since 0.1.0
{-# INLINE name #-}
name :: Char -> Maybe String
name (C# c#) = case indexCharOffAddr# name# 0# of
    '\0'# -> Nothing
    _     -> Just (unpackCString# name#)
    where name# = DerivedName.name c#

-- | Returns /corrected/ name of a character (see 'NameAliases.Correction'),
-- if defined, otherwise returns its original 'name' if defined.
--
-- @since 0.1.0
{-# INLINE correctedName #-}
correctedName :: Char -> Maybe String
correctedName c@(C# c#) = corrected <|> name c
    where
    -- Assumption: fromEnum NameAliases.Correction == 0
    corrected = case indexCharOffAddr# addr# 0# of
        '\xff'# -> Nothing -- no aliases
        '\x00'# -> Nothing -- no correction
        i#      -> Just (unpackCString# (addr# `plusAddr#` (ord# i# +# 1#)))
    addr# = NameAliases.nameAliases c#

-- | Returns a character’s 'name' if defined,
-- otherwise returns its first name alias if defined.
--
-- @since 0.1.0
nameOrAlias :: Char -> Maybe String
nameOrAlias c@(C# c#) = name c <|> case indexCharOffAddr# addr# 0# of
    '\xff'# -> Nothing -- no aliases
    _       -> Just (unpackCString# (go 0#))
    where
    addr# = NameAliases.nameAliases c#
    !(I# maxNameAliasType#) = NameAliases.maxNameAliasType
    go t# = case indexCharOffAddr# (addr# `plusAddr#` t#) 0# of
        '\0'# -> if isTrue# (t# <# maxNameAliasType#)
            then go (t# +# 1#)
            else "\0"# -- impossible: there is at least one alias
        i# -> addr# `plusAddr#` (ord# i# +# 1#)

-- | All name aliases of a character, if defined.
-- The names are listed in the original order of the UCD.
--
-- See 'nameAliasesWithTypes' for the detailed list by alias type.
--
-- @since 0.1.0
-- {-# INLINE nameAliases #-}
-- nameAliases :: Char -> [String]
-- nameAliases = fmap unpack . NameAliases.nameAliases

-- | Name aliases of a character for a specific name alias type.
--
-- @since 0.1.0
{-# INLINE nameAliasesByType #-}
nameAliasesByType :: NameAliases.NameAliasType -> Char -> [String]
-- nameAliasesByType t = fmap unpack . NameAliases.nameAliasesByType t
nameAliasesByType t (C# c#) = case indexCharOffAddr# addr# 0# of
    '\xff'# -> [] -- no aliases
    _       -> nameAliasesByType# addr# t
    where
    addr# = NameAliases.nameAliases c#

-- | Detailed character names aliases.
-- The names are listed in the original order of the UCD.
--
-- See 'nameAliases' if the alias type is not required.
--
-- @since 0.1.0
{-# INLINE nameAliasesWithTypes #-}
nameAliasesWithTypes :: Char -> [(NameAliases.NameAliasType, [String])]
nameAliasesWithTypes (C# c#) = case indexCharOffAddr# addr# 0# of
    '\xff'# -> [] -- no aliases
    _       -> foldr mk mempty [minBound..maxBound]
    where
    addr# = NameAliases.nameAliases c#
    mk t acc = case nameAliasesByType# addr# t of
        [] -> acc
        as -> (t, as) : acc

{-# INLINE nameAliasesByType# #-}
nameAliasesByType# :: Addr# -> NameAliases.NameAliasType -> [String]
nameAliasesByType# addr# t = case indexCharOffAddr# (addr# `plusAddr#` t#) 0# of
    '\0'# -> [] -- no aliases for this type
    i#    -> unpackCStrings addr# (ord# i#)
    where t# = dataToTag# t

{-# INLINE unpackCStrings #-}
unpackCStrings :: Addr# -> Int# -> [String]
unpackCStrings addr# = go
    where
    go i#
        = unpackCString# (addr# `plusAddr#` (i# +# 1#))
        : case indexCharOffAddr# (addr# `plusAddr#` i#) 0# of
            '\0'# -> []
            j#    -> go (ord# j#)


