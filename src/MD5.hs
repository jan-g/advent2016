module MD5 (hash) where

import Data.Function ((&))
import qualified Crypto.Hash.MD5 as OrigMD5
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BSU

hash str = -- trace (show idx) $
         let cs@[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] = OrigMD5.hash (BSU.fromString str) & B.unpack
         in -- trace ("hash is " ++ show cs) $
            map toChar [ a `div` 16, a `mod` 16,
                         b `div` 16, b `mod` 16,
                         c `div` 16, c `mod` 16,
                         d `div` 16, d `mod` 16,
                         e `div` 16, e `mod` 16,
                         f `div` 16, f `mod` 16,
                         g `div` 16, g `mod` 16,
                         h `div` 16, h `mod` 16,
                         i `div` 16, i `mod` 16,
                         j `div` 16, j `mod` 16,
                         k `div` 16, k `mod` 16,
                         l `div` 16, l `mod` 16,
                         m `div` 16, m `mod` 16,
                         n `div` 16, n `mod` 16,
                         o `div` 16, o `mod` 16,
                         p `div` 16, p `mod` 16 ]

toChar 0 = '0'
toChar 1 = '1'
toChar 2 = '2'
toChar 3 = '3'
toChar 4 = '4'
toChar 5 = '5'
toChar 6 = '6'
toChar 7 = '7'
toChar 8 = '8'
toChar 9 = '9'
toChar 10 = 'a'
toChar 11 = 'b'
toChar 12 = 'c'
toChar 13 = 'd'
toChar 14 = 'e'
toChar 15 = 'f'

