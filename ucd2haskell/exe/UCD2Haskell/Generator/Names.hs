-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Generator.Names
    ( generateModules
    ) where

import System.FilePath ((</>))
import qualified Unicode.CharacterDatabase.Parser.Extracted.DerivedName as N
import qualified Unicode.CharacterDatabase.Parser.NameAliases as NA

import qualified UCD2Haskell.Modules.UnicodeData.DerivedNames as Names
import qualified UCD2Haskell.Modules.UnicodeData.NameAliases as NameAliases
import UCD2Haskell.Generator (runGenerator)

generateModules :: FilePath -> FilePath -> IO ()
generateModules indir outdir = do
    runGenerator
        indir
        ("extracted" </> "DerivedName.txt")
        N.parse
        outdir
        [ Names.recipe ]

    runGenerator
        indir
        "NameAliases.txt"
        NA.parse
        outdir
        [ NameAliases.recipe ]