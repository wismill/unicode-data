-- autogenerated from https://www.unicode.org/Public/15.0.0/ucd/NameAliases.txt
-- |
-- Module      : Unicode.Internal.Char.UnicodeData.NameAliases
-- Copyright   : (c) 2022 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental

{-# OPTIONS_HADDOCK hide #-}

module Unicode.Internal.Char.UnicodeData.NameAliases
(NameAliasType(..), nameAliases, nameAliasesByType, nameAliasesWithTypes)
where

import Data.Ix (Ix)
import Data.Maybe (fromMaybe)
import Foreign.C.String (CString)
import GHC.Exts (Ptr(..))

-- | Type of name alias. See Unicode Standard 15.0.0, section 4.8.
--
-- @since 0.1.0
data NameAliasType
    = Correction
    -- ^ Corrections for serious problems in the character names.
    | Control
    -- ^ ISO&#xa0;6429 names for @C0@ and @C1@ control functions, and other
    --   commonly occurring names for control codes.
    | Alternate
    -- ^ A few widely used alternate names for format characters.
    | Figment
    -- ^ Several documented labels for @C1@ control code points which
    --   were never actually approved in any standard.
    | Abbreviation
    -- ^ Commonly occurring abbreviations (or acronyms) for control codes,
    --   format characters, spaces, and variation selectors.
    deriving (Enum, Bounded, Eq, Ord, Ix, Show)

-- | All name aliases of a character.
-- The names are listed in the original order of the UCD.
--
-- See 'nameAliasesWithTypes' for the detailed list by alias type.
--
-- @since 0.1.0
{-# INLINE nameAliases #-}
nameAliases :: Char -> [CString]
nameAliases = mconcat . fmap snd . nameAliasesWithTypes

-- | Name aliases of a character for a specific name alias type.
--
-- @since 0.1.0
{-# INLINE nameAliasesByType #-}
nameAliasesByType :: NameAliasType -> Char -> [CString]
nameAliasesByType t = fromMaybe mempty . lookup t . nameAliasesWithTypes

-- | Detailed character names aliases.
-- The names are listed in the original order of the UCD.
--
-- See 'nameAliases' if the alias type is not required.
--
-- @since 0.1.0
nameAliasesWithTypes :: Char -> [(NameAliasType, [CString])]
nameAliasesWithTypes = \case
  '\x0000' -> [(Control,[Ptr "NULL\0"#]),(Abbreviation,[Ptr "NUL\0"#])]
  '\x0001' -> [(Control,[Ptr "START OF HEADING\0"#]),(Abbreviation,[Ptr "SOH\0"#])]
  '\x0002' -> [(Control,[Ptr "START OF TEXT\0"#]),(Abbreviation,[Ptr "STX\0"#])]
  '\x0003' -> [(Control,[Ptr "END OF TEXT\0"#]),(Abbreviation,[Ptr "ETX\0"#])]
  '\x0004' -> [(Control,[Ptr "END OF TRANSMISSION\0"#]),(Abbreviation,[Ptr "EOT\0"#])]
  '\x0005' -> [(Control,[Ptr "ENQUIRY\0"#]),(Abbreviation,[Ptr "ENQ\0"#])]
  '\x0006' -> [(Control,[Ptr "ACKNOWLEDGE\0"#]),(Abbreviation,[Ptr "ACK\0"#])]
  '\x0007' -> [(Control,[Ptr "ALERT\0"#]),(Abbreviation,[Ptr "BEL\0"#])]
  '\x0008' -> [(Control,[Ptr "BACKSPACE\0"#]),(Abbreviation,[Ptr "BS\0"#])]
  '\x0009' -> [(Control,[Ptr "CHARACTER TABULATION\0"#,Ptr "HORIZONTAL TABULATION\0"#]),(Abbreviation,[Ptr "HT\0"#,Ptr "TAB\0"#])]
  '\x000A' -> [(Control,[Ptr "LINE FEED\0"#,Ptr "NEW LINE\0"#,Ptr "END OF LINE\0"#]),(Abbreviation,[Ptr "LF\0"#,Ptr "NL\0"#,Ptr "EOL\0"#])]
  '\x000B' -> [(Control,[Ptr "LINE TABULATION\0"#,Ptr "VERTICAL TABULATION\0"#]),(Abbreviation,[Ptr "VT\0"#])]
  '\x000C' -> [(Control,[Ptr "FORM FEED\0"#]),(Abbreviation,[Ptr "FF\0"#])]
  '\x000D' -> [(Control,[Ptr "CARRIAGE RETURN\0"#]),(Abbreviation,[Ptr "CR\0"#])]
  '\x000E' -> [(Control,[Ptr "SHIFT OUT\0"#,Ptr "LOCKING-SHIFT ONE\0"#]),(Abbreviation,[Ptr "SO\0"#])]
  '\x000F' -> [(Control,[Ptr "SHIFT IN\0"#,Ptr "LOCKING-SHIFT ZERO\0"#]),(Abbreviation,[Ptr "SI\0"#])]
  '\x0010' -> [(Control,[Ptr "DATA LINK ESCAPE\0"#]),(Abbreviation,[Ptr "DLE\0"#])]
  '\x0011' -> [(Control,[Ptr "DEVICE CONTROL ONE\0"#]),(Abbreviation,[Ptr "DC1\0"#])]
  '\x0012' -> [(Control,[Ptr "DEVICE CONTROL TWO\0"#]),(Abbreviation,[Ptr "DC2\0"#])]
  '\x0013' -> [(Control,[Ptr "DEVICE CONTROL THREE\0"#]),(Abbreviation,[Ptr "DC3\0"#])]
  '\x0014' -> [(Control,[Ptr "DEVICE CONTROL FOUR\0"#]),(Abbreviation,[Ptr "DC4\0"#])]
  '\x0015' -> [(Control,[Ptr "NEGATIVE ACKNOWLEDGE\0"#]),(Abbreviation,[Ptr "NAK\0"#])]
  '\x0016' -> [(Control,[Ptr "SYNCHRONOUS IDLE\0"#]),(Abbreviation,[Ptr "SYN\0"#])]
  '\x0017' -> [(Control,[Ptr "END OF TRANSMISSION BLOCK\0"#]),(Abbreviation,[Ptr "ETB\0"#])]
  '\x0018' -> [(Control,[Ptr "CANCEL\0"#]),(Abbreviation,[Ptr "CAN\0"#])]
  '\x0019' -> [(Control,[Ptr "END OF MEDIUM\0"#]),(Abbreviation,[Ptr "EOM\0"#,Ptr "EM\0"#])]
  '\x001A' -> [(Control,[Ptr "SUBSTITUTE\0"#]),(Abbreviation,[Ptr "SUB\0"#])]
  '\x001B' -> [(Control,[Ptr "ESCAPE\0"#]),(Abbreviation,[Ptr "ESC\0"#])]
  '\x001C' -> [(Control,[Ptr "INFORMATION SEPARATOR FOUR\0"#,Ptr "FILE SEPARATOR\0"#]),(Abbreviation,[Ptr "FS\0"#])]
  '\x001D' -> [(Control,[Ptr "INFORMATION SEPARATOR THREE\0"#,Ptr "GROUP SEPARATOR\0"#]),(Abbreviation,[Ptr "GS\0"#])]
  '\x001E' -> [(Control,[Ptr "INFORMATION SEPARATOR TWO\0"#,Ptr "RECORD SEPARATOR\0"#]),(Abbreviation,[Ptr "RS\0"#])]
  '\x001F' -> [(Control,[Ptr "INFORMATION SEPARATOR ONE\0"#,Ptr "UNIT SEPARATOR\0"#]),(Abbreviation,[Ptr "US\0"#])]
  '\x0020' -> [(Abbreviation,[Ptr "SP\0"#])]
  '\x007F' -> [(Control,[Ptr "DELETE\0"#]),(Abbreviation,[Ptr "DEL\0"#])]
  '\x0080' -> [(Figment,[Ptr "PADDING CHARACTER\0"#]),(Abbreviation,[Ptr "PAD\0"#])]
  '\x0081' -> [(Figment,[Ptr "HIGH OCTET PRESET\0"#]),(Abbreviation,[Ptr "HOP\0"#])]
  '\x0082' -> [(Control,[Ptr "BREAK PERMITTED HERE\0"#]),(Abbreviation,[Ptr "BPH\0"#])]
  '\x0083' -> [(Control,[Ptr "NO BREAK HERE\0"#]),(Abbreviation,[Ptr "NBH\0"#])]
  '\x0084' -> [(Control,[Ptr "INDEX\0"#]),(Abbreviation,[Ptr "IND\0"#])]
  '\x0085' -> [(Control,[Ptr "NEXT LINE\0"#]),(Abbreviation,[Ptr "NEL\0"#])]
  '\x0086' -> [(Control,[Ptr "START OF SELECTED AREA\0"#]),(Abbreviation,[Ptr "SSA\0"#])]
  '\x0087' -> [(Control,[Ptr "END OF SELECTED AREA\0"#]),(Abbreviation,[Ptr "ESA\0"#])]
  '\x0088' -> [(Control,[Ptr "CHARACTER TABULATION SET\0"#,Ptr "HORIZONTAL TABULATION SET\0"#]),(Abbreviation,[Ptr "HTS\0"#])]
  '\x0089' -> [(Control,[Ptr "CHARACTER TABULATION WITH JUSTIFICATION\0"#,Ptr "HORIZONTAL TABULATION WITH JUSTIFICATION\0"#]),(Abbreviation,[Ptr "HTJ\0"#])]
  '\x008A' -> [(Control,[Ptr "LINE TABULATION SET\0"#,Ptr "VERTICAL TABULATION SET\0"#]),(Abbreviation,[Ptr "VTS\0"#])]
  '\x008B' -> [(Control,[Ptr "PARTIAL LINE FORWARD\0"#,Ptr "PARTIAL LINE DOWN\0"#]),(Abbreviation,[Ptr "PLD\0"#])]
  '\x008C' -> [(Control,[Ptr "PARTIAL LINE BACKWARD\0"#,Ptr "PARTIAL LINE UP\0"#]),(Abbreviation,[Ptr "PLU\0"#])]
  '\x008D' -> [(Control,[Ptr "REVERSE LINE FEED\0"#,Ptr "REVERSE INDEX\0"#]),(Abbreviation,[Ptr "RI\0"#])]
  '\x008E' -> [(Control,[Ptr "SINGLE SHIFT TWO\0"#,Ptr "SINGLE-SHIFT-2\0"#]),(Abbreviation,[Ptr "SS2\0"#])]
  '\x008F' -> [(Control,[Ptr "SINGLE SHIFT THREE\0"#,Ptr "SINGLE-SHIFT-3\0"#]),(Abbreviation,[Ptr "SS3\0"#])]
  '\x0090' -> [(Control,[Ptr "DEVICE CONTROL STRING\0"#]),(Abbreviation,[Ptr "DCS\0"#])]
  '\x0091' -> [(Control,[Ptr "PRIVATE USE ONE\0"#,Ptr "PRIVATE USE-1\0"#]),(Abbreviation,[Ptr "PU1\0"#])]
  '\x0092' -> [(Control,[Ptr "PRIVATE USE TWO\0"#,Ptr "PRIVATE USE-2\0"#]),(Abbreviation,[Ptr "PU2\0"#])]
  '\x0093' -> [(Control,[Ptr "SET TRANSMIT STATE\0"#]),(Abbreviation,[Ptr "STS\0"#])]
  '\x0094' -> [(Control,[Ptr "CANCEL CHARACTER\0"#]),(Abbreviation,[Ptr "CCH\0"#])]
  '\x0095' -> [(Control,[Ptr "MESSAGE WAITING\0"#]),(Abbreviation,[Ptr "MW\0"#])]
  '\x0096' -> [(Control,[Ptr "START OF GUARDED AREA\0"#,Ptr "START OF PROTECTED AREA\0"#]),(Abbreviation,[Ptr "SPA\0"#])]
  '\x0097' -> [(Control,[Ptr "END OF GUARDED AREA\0"#,Ptr "END OF PROTECTED AREA\0"#]),(Abbreviation,[Ptr "EPA\0"#])]
  '\x0098' -> [(Control,[Ptr "START OF STRING\0"#]),(Abbreviation,[Ptr "SOS\0"#])]
  '\x0099' -> [(Figment,[Ptr "SINGLE GRAPHIC CHARACTER INTRODUCER\0"#]),(Abbreviation,[Ptr "SGC\0"#])]
  '\x009A' -> [(Control,[Ptr "SINGLE CHARACTER INTRODUCER\0"#]),(Abbreviation,[Ptr "SCI\0"#])]
  '\x009B' -> [(Control,[Ptr "CONTROL SEQUENCE INTRODUCER\0"#]),(Abbreviation,[Ptr "CSI\0"#])]
  '\x009C' -> [(Control,[Ptr "STRING TERMINATOR\0"#]),(Abbreviation,[Ptr "ST\0"#])]
  '\x009D' -> [(Control,[Ptr "OPERATING SYSTEM COMMAND\0"#]),(Abbreviation,[Ptr "OSC\0"#])]
  '\x009E' -> [(Control,[Ptr "PRIVACY MESSAGE\0"#]),(Abbreviation,[Ptr "PM\0"#])]
  '\x009F' -> [(Control,[Ptr "APPLICATION PROGRAM COMMAND\0"#]),(Abbreviation,[Ptr "APC\0"#])]
  '\x00A0' -> [(Abbreviation,[Ptr "NBSP\0"#])]
  '\x00AD' -> [(Abbreviation,[Ptr "SHY\0"#])]
  '\x01A2' -> [(Correction,[Ptr "LATIN CAPITAL LETTER GHA\0"#])]
  '\x01A3' -> [(Correction,[Ptr "LATIN SMALL LETTER GHA\0"#])]
  '\x034F' -> [(Abbreviation,[Ptr "CGJ\0"#])]
  '\x0616' -> [(Correction,[Ptr "ARABIC SMALL HIGH LIGATURE ALEF WITH YEH BARREE\0"#])]
  '\x061C' -> [(Abbreviation,[Ptr "ALM\0"#])]
  '\x0709' -> [(Correction,[Ptr "SYRIAC SUBLINEAR COLON SKEWED LEFT\0"#])]
  '\x0CDE' -> [(Correction,[Ptr "KANNADA LETTER LLLA\0"#])]
  '\x0E9D' -> [(Correction,[Ptr "LAO LETTER FO FON\0"#])]
  '\x0E9F' -> [(Correction,[Ptr "LAO LETTER FO FAY\0"#])]
  '\x0EA3' -> [(Correction,[Ptr "LAO LETTER RO\0"#])]
  '\x0EA5' -> [(Correction,[Ptr "LAO LETTER LO\0"#])]
  '\x0FD0' -> [(Correction,[Ptr "TIBETAN MARK BKA- SHOG GI MGO RGYAN\0"#])]
  '\x11EC' -> [(Correction,[Ptr "HANGUL JONGSEONG YESIEUNG-KIYEOK\0"#])]
  '\x11ED' -> [(Correction,[Ptr "HANGUL JONGSEONG YESIEUNG-SSANGKIYEOK\0"#])]
  '\x11EE' -> [(Correction,[Ptr "HANGUL JONGSEONG SSANGYESIEUNG\0"#])]
  '\x11EF' -> [(Correction,[Ptr "HANGUL JONGSEONG YESIEUNG-KHIEUKH\0"#])]
  '\x180B' -> [(Abbreviation,[Ptr "FVS1\0"#])]
  '\x180C' -> [(Abbreviation,[Ptr "FVS2\0"#])]
  '\x180D' -> [(Abbreviation,[Ptr "FVS3\0"#])]
  '\x180E' -> [(Abbreviation,[Ptr "MVS\0"#])]
  '\x180F' -> [(Abbreviation,[Ptr "FVS4\0"#])]
  '\x1BBD' -> [(Correction,[Ptr "SUNDANESE LETTER ARCHAIC I\0"#])]
  '\x200B' -> [(Abbreviation,[Ptr "ZWSP\0"#])]
  '\x200C' -> [(Abbreviation,[Ptr "ZWNJ\0"#])]
  '\x200D' -> [(Abbreviation,[Ptr "ZWJ\0"#])]
  '\x200E' -> [(Abbreviation,[Ptr "LRM\0"#])]
  '\x200F' -> [(Abbreviation,[Ptr "RLM\0"#])]
  '\x202A' -> [(Abbreviation,[Ptr "LRE\0"#])]
  '\x202B' -> [(Abbreviation,[Ptr "RLE\0"#])]
  '\x202C' -> [(Abbreviation,[Ptr "PDF\0"#])]
  '\x202D' -> [(Abbreviation,[Ptr "LRO\0"#])]
  '\x202E' -> [(Abbreviation,[Ptr "RLO\0"#])]
  '\x202F' -> [(Abbreviation,[Ptr "NNBSP\0"#])]
  '\x205F' -> [(Abbreviation,[Ptr "MMSP\0"#])]
  '\x2060' -> [(Abbreviation,[Ptr "WJ\0"#])]
  '\x2066' -> [(Abbreviation,[Ptr "LRI\0"#])]
  '\x2067' -> [(Abbreviation,[Ptr "RLI\0"#])]
  '\x2068' -> [(Abbreviation,[Ptr "FSI\0"#])]
  '\x2069' -> [(Abbreviation,[Ptr "PDI\0"#])]
  '\x2118' -> [(Correction,[Ptr "WEIERSTRASS ELLIPTIC FUNCTION\0"#])]
  '\x2448' -> [(Correction,[Ptr "MICR ON US SYMBOL\0"#])]
  '\x2449' -> [(Correction,[Ptr "MICR DASH SYMBOL\0"#])]
  '\x2B7A' -> [(Correction,[Ptr "LEFTWARDS TRIANGLE-HEADED ARROW WITH DOUBLE VERTICAL STROKE\0"#])]
  '\x2B7C' -> [(Correction,[Ptr "RIGHTWARDS TRIANGLE-HEADED ARROW WITH DOUBLE VERTICAL STROKE\0"#])]
  '\xA015' -> [(Correction,[Ptr "YI SYLLABLE ITERATION MARK\0"#])]
  '\xAA6E' -> [(Correction,[Ptr "MYANMAR LETTER KHAMTI LLA\0"#])]
  '\xFE00' -> [(Abbreviation,[Ptr "VS1\0"#])]
  '\xFE01' -> [(Abbreviation,[Ptr "VS2\0"#])]
  '\xFE02' -> [(Abbreviation,[Ptr "VS3\0"#])]
  '\xFE03' -> [(Abbreviation,[Ptr "VS4\0"#])]
  '\xFE04' -> [(Abbreviation,[Ptr "VS5\0"#])]
  '\xFE05' -> [(Abbreviation,[Ptr "VS6\0"#])]
  '\xFE06' -> [(Abbreviation,[Ptr "VS7\0"#])]
  '\xFE07' -> [(Abbreviation,[Ptr "VS8\0"#])]
  '\xFE08' -> [(Abbreviation,[Ptr "VS9\0"#])]
  '\xFE09' -> [(Abbreviation,[Ptr "VS10\0"#])]
  '\xFE0A' -> [(Abbreviation,[Ptr "VS11\0"#])]
  '\xFE0B' -> [(Abbreviation,[Ptr "VS12\0"#])]
  '\xFE0C' -> [(Abbreviation,[Ptr "VS13\0"#])]
  '\xFE0D' -> [(Abbreviation,[Ptr "VS14\0"#])]
  '\xFE0E' -> [(Abbreviation,[Ptr "VS15\0"#])]
  '\xFE0F' -> [(Abbreviation,[Ptr "VS16\0"#])]
  '\xFE18' -> [(Correction,[Ptr "PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRACKET\0"#])]
  '\xFEFF' -> [(Alternate,[Ptr "BYTE ORDER MARK\0"#]),(Abbreviation,[Ptr "BOM\0"#,Ptr "ZWNBSP\0"#])]
  '\x122D4' -> [(Correction,[Ptr "CUNEIFORM SIGN NU11 TENU\0"#])]
  '\x122D5' -> [(Correction,[Ptr "CUNEIFORM SIGN NU11 OVER NU11 BUR OVER BUR\0"#])]
  '\x16E56' -> [(Correction,[Ptr "MEDEFAIDRIN CAPITAL LETTER H\0"#])]
  '\x16E57' -> [(Correction,[Ptr "MEDEFAIDRIN CAPITAL LETTER NG\0"#])]
  '\x16E76' -> [(Correction,[Ptr "MEDEFAIDRIN SMALL LETTER H\0"#])]
  '\x16E77' -> [(Correction,[Ptr "MEDEFAIDRIN SMALL LETTER NG\0"#])]
  '\x1B001' -> [(Correction,[Ptr "HENTAIGANA LETTER E-1\0"#])]
  '\x1D0C5' -> [(Correction,[Ptr "BYZANTINE MUSICAL SYMBOL FTHORA SKLIRON CHROMA VASIS\0"#])]
  '\xE0100' -> [(Abbreviation,[Ptr "VS17\0"#])]
  '\xE0101' -> [(Abbreviation,[Ptr "VS18\0"#])]
  '\xE0102' -> [(Abbreviation,[Ptr "VS19\0"#])]
  '\xE0103' -> [(Abbreviation,[Ptr "VS20\0"#])]
  '\xE0104' -> [(Abbreviation,[Ptr "VS21\0"#])]
  '\xE0105' -> [(Abbreviation,[Ptr "VS22\0"#])]
  '\xE0106' -> [(Abbreviation,[Ptr "VS23\0"#])]
  '\xE0107' -> [(Abbreviation,[Ptr "VS24\0"#])]
  '\xE0108' -> [(Abbreviation,[Ptr "VS25\0"#])]
  '\xE0109' -> [(Abbreviation,[Ptr "VS26\0"#])]
  '\xE010A' -> [(Abbreviation,[Ptr "VS27\0"#])]
  '\xE010B' -> [(Abbreviation,[Ptr "VS28\0"#])]
  '\xE010C' -> [(Abbreviation,[Ptr "VS29\0"#])]
  '\xE010D' -> [(Abbreviation,[Ptr "VS30\0"#])]
  '\xE010E' -> [(Abbreviation,[Ptr "VS31\0"#])]
  '\xE010F' -> [(Abbreviation,[Ptr "VS32\0"#])]
  '\xE0110' -> [(Abbreviation,[Ptr "VS33\0"#])]
  '\xE0111' -> [(Abbreviation,[Ptr "VS34\0"#])]
  '\xE0112' -> [(Abbreviation,[Ptr "VS35\0"#])]
  '\xE0113' -> [(Abbreviation,[Ptr "VS36\0"#])]
  '\xE0114' -> [(Abbreviation,[Ptr "VS37\0"#])]
  '\xE0115' -> [(Abbreviation,[Ptr "VS38\0"#])]
  '\xE0116' -> [(Abbreviation,[Ptr "VS39\0"#])]
  '\xE0117' -> [(Abbreviation,[Ptr "VS40\0"#])]
  '\xE0118' -> [(Abbreviation,[Ptr "VS41\0"#])]
  '\xE0119' -> [(Abbreviation,[Ptr "VS42\0"#])]
  '\xE011A' -> [(Abbreviation,[Ptr "VS43\0"#])]
  '\xE011B' -> [(Abbreviation,[Ptr "VS44\0"#])]
  '\xE011C' -> [(Abbreviation,[Ptr "VS45\0"#])]
  '\xE011D' -> [(Abbreviation,[Ptr "VS46\0"#])]
  '\xE011E' -> [(Abbreviation,[Ptr "VS47\0"#])]
  '\xE011F' -> [(Abbreviation,[Ptr "VS48\0"#])]
  '\xE0120' -> [(Abbreviation,[Ptr "VS49\0"#])]
  '\xE0121' -> [(Abbreviation,[Ptr "VS50\0"#])]
  '\xE0122' -> [(Abbreviation,[Ptr "VS51\0"#])]
  '\xE0123' -> [(Abbreviation,[Ptr "VS52\0"#])]
  '\xE0124' -> [(Abbreviation,[Ptr "VS53\0"#])]
  '\xE0125' -> [(Abbreviation,[Ptr "VS54\0"#])]
  '\xE0126' -> [(Abbreviation,[Ptr "VS55\0"#])]
  '\xE0127' -> [(Abbreviation,[Ptr "VS56\0"#])]
  '\xE0128' -> [(Abbreviation,[Ptr "VS57\0"#])]
  '\xE0129' -> [(Abbreviation,[Ptr "VS58\0"#])]
  '\xE012A' -> [(Abbreviation,[Ptr "VS59\0"#])]
  '\xE012B' -> [(Abbreviation,[Ptr "VS60\0"#])]
  '\xE012C' -> [(Abbreviation,[Ptr "VS61\0"#])]
  '\xE012D' -> [(Abbreviation,[Ptr "VS62\0"#])]
  '\xE012E' -> [(Abbreviation,[Ptr "VS63\0"#])]
  '\xE012F' -> [(Abbreviation,[Ptr "VS64\0"#])]
  '\xE0130' -> [(Abbreviation,[Ptr "VS65\0"#])]
  '\xE0131' -> [(Abbreviation,[Ptr "VS66\0"#])]
  '\xE0132' -> [(Abbreviation,[Ptr "VS67\0"#])]
  '\xE0133' -> [(Abbreviation,[Ptr "VS68\0"#])]
  '\xE0134' -> [(Abbreviation,[Ptr "VS69\0"#])]
  '\xE0135' -> [(Abbreviation,[Ptr "VS70\0"#])]
  '\xE0136' -> [(Abbreviation,[Ptr "VS71\0"#])]
  '\xE0137' -> [(Abbreviation,[Ptr "VS72\0"#])]
  '\xE0138' -> [(Abbreviation,[Ptr "VS73\0"#])]
  '\xE0139' -> [(Abbreviation,[Ptr "VS74\0"#])]
  '\xE013A' -> [(Abbreviation,[Ptr "VS75\0"#])]
  '\xE013B' -> [(Abbreviation,[Ptr "VS76\0"#])]
  '\xE013C' -> [(Abbreviation,[Ptr "VS77\0"#])]
  '\xE013D' -> [(Abbreviation,[Ptr "VS78\0"#])]
  '\xE013E' -> [(Abbreviation,[Ptr "VS79\0"#])]
  '\xE013F' -> [(Abbreviation,[Ptr "VS80\0"#])]
  '\xE0140' -> [(Abbreviation,[Ptr "VS81\0"#])]
  '\xE0141' -> [(Abbreviation,[Ptr "VS82\0"#])]
  '\xE0142' -> [(Abbreviation,[Ptr "VS83\0"#])]
  '\xE0143' -> [(Abbreviation,[Ptr "VS84\0"#])]
  '\xE0144' -> [(Abbreviation,[Ptr "VS85\0"#])]
  '\xE0145' -> [(Abbreviation,[Ptr "VS86\0"#])]
  '\xE0146' -> [(Abbreviation,[Ptr "VS87\0"#])]
  '\xE0147' -> [(Abbreviation,[Ptr "VS88\0"#])]
  '\xE0148' -> [(Abbreviation,[Ptr "VS89\0"#])]
  '\xE0149' -> [(Abbreviation,[Ptr "VS90\0"#])]
  '\xE014A' -> [(Abbreviation,[Ptr "VS91\0"#])]
  '\xE014B' -> [(Abbreviation,[Ptr "VS92\0"#])]
  '\xE014C' -> [(Abbreviation,[Ptr "VS93\0"#])]
  '\xE014D' -> [(Abbreviation,[Ptr "VS94\0"#])]
  '\xE014E' -> [(Abbreviation,[Ptr "VS95\0"#])]
  '\xE014F' -> [(Abbreviation,[Ptr "VS96\0"#])]
  '\xE0150' -> [(Abbreviation,[Ptr "VS97\0"#])]
  '\xE0151' -> [(Abbreviation,[Ptr "VS98\0"#])]
  '\xE0152' -> [(Abbreviation,[Ptr "VS99\0"#])]
  '\xE0153' -> [(Abbreviation,[Ptr "VS100\0"#])]
  '\xE0154' -> [(Abbreviation,[Ptr "VS101\0"#])]
  '\xE0155' -> [(Abbreviation,[Ptr "VS102\0"#])]
  '\xE0156' -> [(Abbreviation,[Ptr "VS103\0"#])]
  '\xE0157' -> [(Abbreviation,[Ptr "VS104\0"#])]
  '\xE0158' -> [(Abbreviation,[Ptr "VS105\0"#])]
  '\xE0159' -> [(Abbreviation,[Ptr "VS106\0"#])]
  '\xE015A' -> [(Abbreviation,[Ptr "VS107\0"#])]
  '\xE015B' -> [(Abbreviation,[Ptr "VS108\0"#])]
  '\xE015C' -> [(Abbreviation,[Ptr "VS109\0"#])]
  '\xE015D' -> [(Abbreviation,[Ptr "VS110\0"#])]
  '\xE015E' -> [(Abbreviation,[Ptr "VS111\0"#])]
  '\xE015F' -> [(Abbreviation,[Ptr "VS112\0"#])]
  '\xE0160' -> [(Abbreviation,[Ptr "VS113\0"#])]
  '\xE0161' -> [(Abbreviation,[Ptr "VS114\0"#])]
  '\xE0162' -> [(Abbreviation,[Ptr "VS115\0"#])]
  '\xE0163' -> [(Abbreviation,[Ptr "VS116\0"#])]
  '\xE0164' -> [(Abbreviation,[Ptr "VS117\0"#])]
  '\xE0165' -> [(Abbreviation,[Ptr "VS118\0"#])]
  '\xE0166' -> [(Abbreviation,[Ptr "VS119\0"#])]
  '\xE0167' -> [(Abbreviation,[Ptr "VS120\0"#])]
  '\xE0168' -> [(Abbreviation,[Ptr "VS121\0"#])]
  '\xE0169' -> [(Abbreviation,[Ptr "VS122\0"#])]
  '\xE016A' -> [(Abbreviation,[Ptr "VS123\0"#])]
  '\xE016B' -> [(Abbreviation,[Ptr "VS124\0"#])]
  '\xE016C' -> [(Abbreviation,[Ptr "VS125\0"#])]
  '\xE016D' -> [(Abbreviation,[Ptr "VS126\0"#])]
  '\xE016E' -> [(Abbreviation,[Ptr "VS127\0"#])]
  '\xE016F' -> [(Abbreviation,[Ptr "VS128\0"#])]
  '\xE0170' -> [(Abbreviation,[Ptr "VS129\0"#])]
  '\xE0171' -> [(Abbreviation,[Ptr "VS130\0"#])]
  '\xE0172' -> [(Abbreviation,[Ptr "VS131\0"#])]
  '\xE0173' -> [(Abbreviation,[Ptr "VS132\0"#])]
  '\xE0174' -> [(Abbreviation,[Ptr "VS133\0"#])]
  '\xE0175' -> [(Abbreviation,[Ptr "VS134\0"#])]
  '\xE0176' -> [(Abbreviation,[Ptr "VS135\0"#])]
  '\xE0177' -> [(Abbreviation,[Ptr "VS136\0"#])]
  '\xE0178' -> [(Abbreviation,[Ptr "VS137\0"#])]
  '\xE0179' -> [(Abbreviation,[Ptr "VS138\0"#])]
  '\xE017A' -> [(Abbreviation,[Ptr "VS139\0"#])]
  '\xE017B' -> [(Abbreviation,[Ptr "VS140\0"#])]
  '\xE017C' -> [(Abbreviation,[Ptr "VS141\0"#])]
  '\xE017D' -> [(Abbreviation,[Ptr "VS142\0"#])]
  '\xE017E' -> [(Abbreviation,[Ptr "VS143\0"#])]
  '\xE017F' -> [(Abbreviation,[Ptr "VS144\0"#])]
  '\xE0180' -> [(Abbreviation,[Ptr "VS145\0"#])]
  '\xE0181' -> [(Abbreviation,[Ptr "VS146\0"#])]
  '\xE0182' -> [(Abbreviation,[Ptr "VS147\0"#])]
  '\xE0183' -> [(Abbreviation,[Ptr "VS148\0"#])]
  '\xE0184' -> [(Abbreviation,[Ptr "VS149\0"#])]
  '\xE0185' -> [(Abbreviation,[Ptr "VS150\0"#])]
  '\xE0186' -> [(Abbreviation,[Ptr "VS151\0"#])]
  '\xE0187' -> [(Abbreviation,[Ptr "VS152\0"#])]
  '\xE0188' -> [(Abbreviation,[Ptr "VS153\0"#])]
  '\xE0189' -> [(Abbreviation,[Ptr "VS154\0"#])]
  '\xE018A' -> [(Abbreviation,[Ptr "VS155\0"#])]
  '\xE018B' -> [(Abbreviation,[Ptr "VS156\0"#])]
  '\xE018C' -> [(Abbreviation,[Ptr "VS157\0"#])]
  '\xE018D' -> [(Abbreviation,[Ptr "VS158\0"#])]
  '\xE018E' -> [(Abbreviation,[Ptr "VS159\0"#])]
  '\xE018F' -> [(Abbreviation,[Ptr "VS160\0"#])]
  '\xE0190' -> [(Abbreviation,[Ptr "VS161\0"#])]
  '\xE0191' -> [(Abbreviation,[Ptr "VS162\0"#])]
  '\xE0192' -> [(Abbreviation,[Ptr "VS163\0"#])]
  '\xE0193' -> [(Abbreviation,[Ptr "VS164\0"#])]
  '\xE0194' -> [(Abbreviation,[Ptr "VS165\0"#])]
  '\xE0195' -> [(Abbreviation,[Ptr "VS166\0"#])]
  '\xE0196' -> [(Abbreviation,[Ptr "VS167\0"#])]
  '\xE0197' -> [(Abbreviation,[Ptr "VS168\0"#])]
  '\xE0198' -> [(Abbreviation,[Ptr "VS169\0"#])]
  '\xE0199' -> [(Abbreviation,[Ptr "VS170\0"#])]
  '\xE019A' -> [(Abbreviation,[Ptr "VS171\0"#])]
  '\xE019B' -> [(Abbreviation,[Ptr "VS172\0"#])]
  '\xE019C' -> [(Abbreviation,[Ptr "VS173\0"#])]
  '\xE019D' -> [(Abbreviation,[Ptr "VS174\0"#])]
  '\xE019E' -> [(Abbreviation,[Ptr "VS175\0"#])]
  '\xE019F' -> [(Abbreviation,[Ptr "VS176\0"#])]
  '\xE01A0' -> [(Abbreviation,[Ptr "VS177\0"#])]
  '\xE01A1' -> [(Abbreviation,[Ptr "VS178\0"#])]
  '\xE01A2' -> [(Abbreviation,[Ptr "VS179\0"#])]
  '\xE01A3' -> [(Abbreviation,[Ptr "VS180\0"#])]
  '\xE01A4' -> [(Abbreviation,[Ptr "VS181\0"#])]
  '\xE01A5' -> [(Abbreviation,[Ptr "VS182\0"#])]
  '\xE01A6' -> [(Abbreviation,[Ptr "VS183\0"#])]
  '\xE01A7' -> [(Abbreviation,[Ptr "VS184\0"#])]
  '\xE01A8' -> [(Abbreviation,[Ptr "VS185\0"#])]
  '\xE01A9' -> [(Abbreviation,[Ptr "VS186\0"#])]
  '\xE01AA' -> [(Abbreviation,[Ptr "VS187\0"#])]
  '\xE01AB' -> [(Abbreviation,[Ptr "VS188\0"#])]
  '\xE01AC' -> [(Abbreviation,[Ptr "VS189\0"#])]
  '\xE01AD' -> [(Abbreviation,[Ptr "VS190\0"#])]
  '\xE01AE' -> [(Abbreviation,[Ptr "VS191\0"#])]
  '\xE01AF' -> [(Abbreviation,[Ptr "VS192\0"#])]
  '\xE01B0' -> [(Abbreviation,[Ptr "VS193\0"#])]
  '\xE01B1' -> [(Abbreviation,[Ptr "VS194\0"#])]
  '\xE01B2' -> [(Abbreviation,[Ptr "VS195\0"#])]
  '\xE01B3' -> [(Abbreviation,[Ptr "VS196\0"#])]
  '\xE01B4' -> [(Abbreviation,[Ptr "VS197\0"#])]
  '\xE01B5' -> [(Abbreviation,[Ptr "VS198\0"#])]
  '\xE01B6' -> [(Abbreviation,[Ptr "VS199\0"#])]
  '\xE01B7' -> [(Abbreviation,[Ptr "VS200\0"#])]
  '\xE01B8' -> [(Abbreviation,[Ptr "VS201\0"#])]
  '\xE01B9' -> [(Abbreviation,[Ptr "VS202\0"#])]
  '\xE01BA' -> [(Abbreviation,[Ptr "VS203\0"#])]
  '\xE01BB' -> [(Abbreviation,[Ptr "VS204\0"#])]
  '\xE01BC' -> [(Abbreviation,[Ptr "VS205\0"#])]
  '\xE01BD' -> [(Abbreviation,[Ptr "VS206\0"#])]
  '\xE01BE' -> [(Abbreviation,[Ptr "VS207\0"#])]
  '\xE01BF' -> [(Abbreviation,[Ptr "VS208\0"#])]
  '\xE01C0' -> [(Abbreviation,[Ptr "VS209\0"#])]
  '\xE01C1' -> [(Abbreviation,[Ptr "VS210\0"#])]
  '\xE01C2' -> [(Abbreviation,[Ptr "VS211\0"#])]
  '\xE01C3' -> [(Abbreviation,[Ptr "VS212\0"#])]
  '\xE01C4' -> [(Abbreviation,[Ptr "VS213\0"#])]
  '\xE01C5' -> [(Abbreviation,[Ptr "VS214\0"#])]
  '\xE01C6' -> [(Abbreviation,[Ptr "VS215\0"#])]
  '\xE01C7' -> [(Abbreviation,[Ptr "VS216\0"#])]
  '\xE01C8' -> [(Abbreviation,[Ptr "VS217\0"#])]
  '\xE01C9' -> [(Abbreviation,[Ptr "VS218\0"#])]
  '\xE01CA' -> [(Abbreviation,[Ptr "VS219\0"#])]
  '\xE01CB' -> [(Abbreviation,[Ptr "VS220\0"#])]
  '\xE01CC' -> [(Abbreviation,[Ptr "VS221\0"#])]
  '\xE01CD' -> [(Abbreviation,[Ptr "VS222\0"#])]
  '\xE01CE' -> [(Abbreviation,[Ptr "VS223\0"#])]
  '\xE01CF' -> [(Abbreviation,[Ptr "VS224\0"#])]
  '\xE01D0' -> [(Abbreviation,[Ptr "VS225\0"#])]
  '\xE01D1' -> [(Abbreviation,[Ptr "VS226\0"#])]
  '\xE01D2' -> [(Abbreviation,[Ptr "VS227\0"#])]
  '\xE01D3' -> [(Abbreviation,[Ptr "VS228\0"#])]
  '\xE01D4' -> [(Abbreviation,[Ptr "VS229\0"#])]
  '\xE01D5' -> [(Abbreviation,[Ptr "VS230\0"#])]
  '\xE01D6' -> [(Abbreviation,[Ptr "VS231\0"#])]
  '\xE01D7' -> [(Abbreviation,[Ptr "VS232\0"#])]
  '\xE01D8' -> [(Abbreviation,[Ptr "VS233\0"#])]
  '\xE01D9' -> [(Abbreviation,[Ptr "VS234\0"#])]
  '\xE01DA' -> [(Abbreviation,[Ptr "VS235\0"#])]
  '\xE01DB' -> [(Abbreviation,[Ptr "VS236\0"#])]
  '\xE01DC' -> [(Abbreviation,[Ptr "VS237\0"#])]
  '\xE01DD' -> [(Abbreviation,[Ptr "VS238\0"#])]
  '\xE01DE' -> [(Abbreviation,[Ptr "VS239\0"#])]
  '\xE01DF' -> [(Abbreviation,[Ptr "VS240\0"#])]
  '\xE01E0' -> [(Abbreviation,[Ptr "VS241\0"#])]
  '\xE01E1' -> [(Abbreviation,[Ptr "VS242\0"#])]
  '\xE01E2' -> [(Abbreviation,[Ptr "VS243\0"#])]
  '\xE01E3' -> [(Abbreviation,[Ptr "VS244\0"#])]
  '\xE01E4' -> [(Abbreviation,[Ptr "VS245\0"#])]
  '\xE01E5' -> [(Abbreviation,[Ptr "VS246\0"#])]
  '\xE01E6' -> [(Abbreviation,[Ptr "VS247\0"#])]
  '\xE01E7' -> [(Abbreviation,[Ptr "VS248\0"#])]
  '\xE01E8' -> [(Abbreviation,[Ptr "VS249\0"#])]
  '\xE01E9' -> [(Abbreviation,[Ptr "VS250\0"#])]
  '\xE01EA' -> [(Abbreviation,[Ptr "VS251\0"#])]
  '\xE01EB' -> [(Abbreviation,[Ptr "VS252\0"#])]
  '\xE01EC' -> [(Abbreviation,[Ptr "VS253\0"#])]
  '\xE01ED' -> [(Abbreviation,[Ptr "VS254\0"#])]
  '\xE01EE' -> [(Abbreviation,[Ptr "VS255\0"#])]
  '\xE01EF' -> [(Abbreviation,[Ptr "VS256\0"#])]

  _ -> mempty
