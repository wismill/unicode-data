#!/bin/sh
# shellcheck disable=SC3043,SC3010,SC3030,SC3054

# When reproducing the Haskell files we want to to be sure that the files that
# we used to generate them earlier are exactly the same as the ones we are
# downloading. To ensure that verfication of the checksum is necessary.

VERSION=18.0.0

# When downloading fresh new version comment this out
VERIFY_CHECKSUM=y

# UCD files (https://www.unicode.org/Public/$VERSION/ucd/$file)
UCD_URL="https://www.unicode.org/Public/$VERSION/ucd"
# Useful command to get the checksums:
# $ find data/$VERSION/ -type f -print0 | xargs -0 sha256sum
# Format: filename:checksum
UCD_FILES="\
    Blocks.txt:2ba23579ee38e0b62b00a1a448f094eb54a823fb96109da685ad261b9bd493f2\
    CaseFolding.txt:a004797658a457bec4dc11683e39f69249ea3b595b752dbea6721c4c9f587b0d\
    DerivedCoreProperties.txt:b040c9b05cd49ca9ecced4066e667abf7513488fa726cc01bbd54948319bdea6\
    DerivedNormalizationProps.txt:98ac7f67d985fe781e317f6182e885e94cabb0c314769e6dd73e48b226931ccd\
    NameAliases.txt:3d5cf5e468901b080cd99adf2230061b748083705fe633db43ac8b73ec7a13da\
    PropertyValueAliases.txt:19a6c4ab639d09a42881507ff1a1055286768f24e9b96d1e810b354a759ec030\
    PropList.txt:ff65a2e6a57992504dad1251281eb9de7f0b223f73c7af9108466b5769141721\
    Scripts.txt:66b1d21a528a1eb8bbfd48e05c2c0808435e90d1787cf5a69752dc88f6e941a0\
    ScriptExtensions.txt:a5dfefc6660af2e34f47269915ca321f58574aa276de2acea9760cd94af96968\
    SpecialCasing.txt:8538dea57c184f1ef3783885ea79677b10f6efa06423717157e63712f14d1ad2\
    UnicodeData.txt:3a30a86cb25ccd95a63baffeabce026ccf59a19d2f7e705acc0a44c1326a5764\
    extracted/DerivedCombiningClass.txt:10048c71ff6860cb707ee21cb7d401a1e50b1169e8e5613018ebca616ed8f5b1\
    extracted/DerivedName.txt:fa05755dfa75395823ce0d8cc0508f645b6bbcf8f59a9468b80586d458b3cd5f\
    extracted/DerivedNumericValues.txt:94e6c8a73e460c4196ab198aec2219cf3bbfd5aae6595f5d048311d41d20c9fb"

# Security files:
# - < 17.0.0: https://www.unicode.org/Public/security/$VERSION/$file)
# - ≥ 17.0.0: https://www.unicode.org/Public/$VERSION/security/$file)
SECURITY_URL="https://www.unicode.org/Public/$VERSION/security"
# Format: filename:checksum
SECURITY_FILES="\
    IdentifierStatus.txt:5863c7d99ca18f213c41c7318aa5528bebfb6d32ec0f1d5944e37192c119aebd\
    IdentifierType.txt:16cd9c65392945904890bb2b64c686397438bcf29b1a47a0fc478a24c17d433a\
    confusables.txt:fa913a52e5ee1106631b0c5489198e36970596fa2d2b3a36e9ed5dfb8f66e4c6\
    intentional.txt:5b69cdfd7be6be45d51b9cf7ec799df91c1acc47c557d66c92a8d6623df78b0e"

# Download the files

# Download $file from https://www.unicode.org/Public/
# and verify the $checksum if $VERIFY_CHECKSUM is enabled
# $1 = file:checksum
download_file() {
    local directory="data/$VERSION/$1"
    local url="$2"
    local pair="$3"
    local file
    local checksum

    file="$(echo "$pair" | cut -f1 -d':')"
    checksum="$(echo "$pair" | cut -f2 -d':')"

    if test ! -e "$directory/$file"
    then
        wget -P "$(dirname "$directory/$file")" "$url/$file"
    fi
    if test -n "$VERIFY_CHECKSUM"
    then
        file="$directory/$file"
        new_checksum=$(sha256sum "$file" | cut -f1 -d' ')
        if test "$checksum" != "$new_checksum"
        then
            echo "sha256sum of the downloaded file $file "
            echo "   [$new_checksum] does not match the expected checksum [$checksum]"
            exit 1
        else
            echo "$file checksum ok"
        fi
    fi
}

# Extract $file from $XXX_FILES, then download it using download_file
download_files() {
    for pair in $3
    do
        download_file "$1" "$2" "$pair"
    done
}

# Generate the Haskell files.
run_generator() {
    # Get remaining arguments to pass to Cabal and ucd2haskell.
    # Split them on “--” and store in arrays to avoid issues with empty strings.
    local cabal_options=()
    local cabal_options_end=false
    local ucd2haskell_opts=()
    for opt in "$@"
    do
        if [ "$cabal_options_end" = true ]; then
            ucd2haskell_opts+=("$opt")
        elif [ "$opt" = "--" ]; then
            cabal_options_end=true
        else
            cabal_options+=("$opt")
        fi
    done

    # Compile and run ucd2haskell
    cabal run --flag ucd2haskell "${cabal_options[@]}" \
        ucd2haskell:ucd2haskell -- \
            --input "./data/$VERSION" \
            --output-core ./unicode-data/lib/ \
            --output-names ./unicode-data-names/lib/ \
            --output-scripts ./unicode-data-scripts/lib/ \
            --output-security ./unicode-data-security/lib/ \
            --core-prop Uppercase \
            --core-prop Lowercase \
            --core-prop Alphabetic \
            --core-prop White_Space \
            --core-prop ID_Start \
            --core-prop ID_Continue \
            --core-prop XID_Start \
            --core-prop XID_Continue \
            --core-prop Pattern_Syntax \
            --core-prop Pattern_White_Space \
            --unicode-version "$VERSION" \
            "${ucd2haskell_opts[@]}"
}

# Print help text
print_help() {
    echo "Usage: ucd.sh <command>"
    echo
    echo "Available commands:"
    echo "  download: downloads the text files required"
    echo "  generate: generate the haskell files from the downloaded text files"
    echo
    echo "Example:"
    echo "$ ./ucd.sh download && ./ucd.sh generate"
    echo
    echo "Further arguments will be passed to cabal."
    echo "The following compiles ucd2haskell with '-O2' and then displays its help."
    echo "$ ./ucd.sh generate -O2 -- --help"
}

# Main program

# Export the version so it can be used by the executable
export UNICODE_VERSION="$VERSION"

# Parse command line
case $1 in
    -h|--help) print_help;;
    download)
        download_files "ucd" "$UCD_URL" "$UCD_FILES";
        download_files "security" "$SECURITY_URL" "$SECURITY_FILES";;
    generate) run_generator "${@:2}";;
    *) echo "Unknown argument"; print_help;;
esac
