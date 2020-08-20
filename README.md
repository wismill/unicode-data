# README

This package provides a library that defines unicode properties and an
executable to generate the contents of the library.

## Unicode database update

Unicode data sources:
* http://www.unicode.org/Public/UCD/latest/
* http://www.unicode.org/Public/UCD/latest/ucd/
* http://www.unicode.org/Public/UCD/latest/ucdxml/

Download and decompress the following files:
* `NormalizationTest.txt` from the `ucd` directory
* `DerivedCoreProperties.txt` from the `ucd` directory
* `ucd.all.flat.zip` from the `ucdxml` directory

```
rm ucd/DerivedCoreProperties.txt
wget -P ucd https://www.unicode.org/Public/UCD/latest/ucd/NormalizationTest.txt
wget -P ucd https://www.unicode.org/Public/UCD/latest/ucd/DerivedCoreProperties.txt
wget -P ucdxml http://www.unicode.org/Public/UCD/latest/ucdxml/ucd.all.flat.zip
unzip -d ucdxml ucdxml/ucd.all.flat.zip
```

## Generating Haskell files from Unicode database

To generate the Haskell data structures from UCD build the ucd2haskell
utility and run it like this:
```
ucd2haskell --ucdxml ./ucdxml/ucd.all.flat.xml --ucd ./ucd --output ../lib/Data/Unicode/Properties --core-prop PROP
```
`PROP = Any *Derived Property*(s) from ucd/DerivedCoreProperties.txt`

To exclude generation from xml files, omit `--ucdxml`

To exclude generation from DerivedCoreProperties.txt, omit `--core-prop`

You can pass in multiple properties like this:
```
ucd2haskell --ucd ./ucd --output ../lib/Data/Unicode/Properties --core-prop Uppercase --core-prop Lowercase
```
Update the unicode version in the changelog below as well as in the top
level README and haddock docs.
