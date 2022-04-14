{-|
Description : provides just a type system related functions and classes.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021
License     : proprietary, to be dual licensed
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module Type provides just a type system related functions and classes.

Suggested import line: 'import qualified Type as T'

The module Type provides:

    * 'Px' - data type to pass type information
-}

module Type
    (
        Px(..), 
        pxFromData
    ) where

-- type Px
{-| ...to pass type information without data.

The type proxy can be used like this:

    @
import qualified Type as T

main :: IO ()
main = 
    do
        print (niFunc (T.Px :: T.Px Char))  -- here the type prox

class XClass x where
    niFunc :: (T.Px x) -> Integer

instance XClass Char where
    niFunc _ = 8

instance XClass Int where
    niFunc _ = 64
    @
-}
data Px a = Px

pxFromData :: a -> Px a
pxFromData _ = Px
