-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module UCD2Haskell.Generator.Core
    ( generateModules
    ) where

import qualified Data.ByteString as B
import qualified Data.Set as Set
import Data.String (IsString(..))
import System.FilePath ((</>))
import qualified Unicode.CharacterDatabase.Parser.CaseFolding as CF
import qualified Unicode.CharacterDatabase.Parser.Extracted.DerivedNumericValues as N
import qualified Unicode.CharacterDatabase.Parser.Properties.Multiple as Props
import qualified Unicode.CharacterDatabase.Parser.Properties.Single as Prop
import qualified Unicode.CharacterDatabase.Parser.UnicodeData as UD

import qualified UCD2Haskell.Modules.Blocks as Blocks
import qualified UCD2Haskell.Modules.CaseFoldings as CaseFoldings
import qualified UCD2Haskell.Modules.DerivedNumericValues as DerivedNumericValues
import qualified UCD2Haskell.Modules.Properties as Properties
import qualified UCD2Haskell.Modules.SpecialCasings as SpecialCasings
import qualified UCD2Haskell.Modules.UnicodeData.CombiningClass as CombiningClass
import qualified UCD2Haskell.Modules.UnicodeData.Composition as Composition
import qualified UCD2Haskell.Modules.UnicodeData.Decomposition as Decomposition
import qualified UCD2Haskell.Modules.UnicodeData.GeneralCategory as GeneralCategory
import qualified UCD2Haskell.Modules.UnicodeData.SimpleCaseMappings as SimpleCaseMappings
import UCD2Haskell.Generator (runGenerator)

generateModules :: FilePath -> FilePath -> [String] -> IO ()
generateModules indir outdir props = do

    fullCompositionExclusion <- Composition.parseFullCompositionExclusion
        <$> B.readFile (indir </> "DerivedNormalizationProps.txt")

    combiningChars <- CombiningClass.parseCombining
        <$> B.readFile (indir </> "extracted" </> "DerivedCombiningClass.txt")

    specialCasings <- SpecialCasings.parse
        <$> B.readFile (indir </> "SpecialCasing.txt")

    runGenerator
        indir
        "Blocks.txt"
        Prop.parse
        outdir
        [ Blocks.recipe ]

    runGenerator
        indir
        "UnicodeData.txt"
        UD.parse
        outdir
        [ Composition.recipe fullCompositionExclusion combiningChars
        , CombiningClass.recipe
        , Decomposition.decomposable
        , Decomposition.decomposableK
        , Decomposition.decompositions
        , Decomposition.decompositionsK2
        , Decomposition.decompositionsK
        , GeneralCategory.recipe
        , SimpleCaseMappings.upperRecipe
        , SimpleCaseMappings.lowerRecipe
        , SimpleCaseMappings.titleRecipe
        , SpecialCasings.upperRecipe specialCasings
        , SpecialCasings.lowerRecipe specialCasings
        , SpecialCasings.titleRecipe specialCasings
        ]

    let propsSet = Set.fromList (fromString <$> props)

    runGenerator
        indir
        "PropList.txt"
        Props.parse
        outdir
        [ Properties.propList propsSet ]

    runGenerator
        indir
        "DerivedCoreProperties.txt"
        Props.parse
        outdir
        [ Properties.derivedCoreProperties propsSet ]

    runGenerator
        indir
        "extracted/DerivedNumericValues.txt"
        N.parse
        outdir
        [ DerivedNumericValues.recipe ]

    runGenerator
        indir
        "CaseFolding.txt"
        CF.parse
        outdir
        [ CaseFoldings.recipe ]