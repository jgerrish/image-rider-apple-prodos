# image-rider-apple-prodos
Haskell Image Rider Parser for Apple ProDOS disk images

## Introduction ##

This ia a Haskell library and application to parse Apple ][ ProDOS
disk images.  It uses the Megaparsec monadic parsr combinator library.

## Usage ##

To parse an image with the main application, run the following:

cabal run image-rider-apple-prodos -- prodos402.dsk

## Development ##

Contributions and bug fixes are welcome.  To run the tests:

cabal test

