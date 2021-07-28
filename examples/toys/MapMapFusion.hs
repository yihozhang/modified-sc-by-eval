{-# LANGUAGE ExtendedDefaultRules, NoMonoPatBinds #-}
module Main(main) where
import Control.DeepSeq
import Data.Time.Clock.POSIX (getPOSIXTime)


time_ :: IO a -> IO Double
time_ act = do { start <- getTime; act; end <- getTime; return $! (Prelude.-) end start }

getTime :: IO Double
getTime = (fromRational . toRational) `fmap` getPOSIXTime

main = do { t <- time_ (rnf results `seq` return ()); print t }
  where results = map assertEq tests

assertEq :: (Show a, Eq a) => (a, a) -> ()
assertEq (x, y) = if x == y then () else error ("FAIL! " ++ show x ++ ", " ++ show y)

root = let
         h0 = \xs_u64 -> h1 xs_u64
         h1 = \xs_u64 -> case xs_u64 of
                           [] -> h2
                           (:) x_u110 xs_u112 -> h3 x_u110 xs_u112
         h2 = []
         h3 = \x_u110 -> \xs_u112 -> let
                                       a_u133 = h4 x_u110
                                       a_u135 = h6 xs_u112
                                     in (:) a_u133 a_u135
         h4 = \x_u110 -> let a_u117 = h5 x_u110
                         in Left a_u117
         h5 = \x_u110 -> Right x_u110
         h6 = \xs_u112 -> case xs_u112 of
                            [] -> h2
                            (:) x_u154 xs_u156 -> h3 x_u154 xs_u156
       in h0
tests = let
          wrap_a10 = \x1 -> \x2 -> (:) x1 x2
          wrap_a8 = \x1 -> Left x1
          wrap_a9 = \x1 -> Right x1
          wrap_a12 = 1 :: Int
          wrap_a14 = 2 :: Int
          wrap_a16 = 3 :: Int
          map = \f -> \xs -> case xs of
                               [] -> []
                               (:) x xs ->
                                 let a_a17 = f x
                                 in let a_a18 = map f xs
                                    in wrap_a10 a_a17 a_a18
          tests = let
                    a_a50 = let
                              a_a46 = let
                                        a_a26 = let
                                                  list_a24 = (:) wrap_a14 list_a21
                                                  list_a21 = (:) wrap_a16 list_a23
                                                  list_a23 = []
                                                in (:) wrap_a12 list_a24
                                      in root a_a26
                              a_a48 = let
                                        a_a43 = let a_a28 = wrap_a9 wrap_a12
                                                in wrap_a8 a_a28
                                        a_a35 = let a_a30 = wrap_a9 wrap_a14
                                                in wrap_a8 a_a30
                                        a_a40 = let a_a32 = wrap_a9 wrap_a16
                                                in wrap_a8 a_a32
                                      in let
                                           list_a44 = (:) a_a35 list_a39
                                           list_a39 = (:) a_a40 list_a42
                                           list_a42 = []
                                         in (:) a_a43 list_a44
                            in (,) a_a46 a_a48
                  in let list_a52 = []
                     in (:) a_a50 list_a52
        in tests
