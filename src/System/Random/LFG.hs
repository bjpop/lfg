{-# LANGUAGE DeriveDataTypeable, CPP, TypeFamilies, FlexibleContexts  #-}

-- | This library implements a Lagged (Additive) Fibonacci Generator.

module System.Random.LFG
   ( Variate (..)
   , Gen
   , GenIO
   , GenST
   , initRng
   , spawnRng
   ) where

import Control.Monad (ap, liftM, when, forM_, replicateM_)
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Data.Vector.Unboxed.Mutable as M
   (unsafeRead, unsafeWrite, unsafeNew, MVector, Unbox)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (ST)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR, xor, complement)
import Data.Primitive.Array (MutableArray, newArray, readArray, writeArray)

type Word32Vec m  = M.MVector (PrimState m) Word32

-- BITS_IN_INT_GEN is the log_2 of the modulus of the generator
-- for portability this is set to 32, but can be modified;
-- if modified, make sure INT_MOD_MASK can still be calculated
bitsInIntGen :: Int
bitsInIntGen = 32

-- INT_MOD_MASK is used to perform modular arithmetic - specifying
-- this value compensates for different sized words on
-- different architectures
intModMask :: Word32
intModMask = 0xffffffff

-- FLT_MULT is used in converting to float and double values
fltMult :: Fractional a => a
fltMult = 0.5 / fromInteger (1 `shiftL` 31)

-- INT_MASK is used to mask out the part of the generator which
-- is not in the canonical form; it should be
-- 2^{BITS_IN_INT_GEN-1}-1
intMask :: Word32
intMask = intModMask `shiftR` 1

-- MAX_BIT_INT is the largest bit position allowed in the index
-- of the node - it equals BITS_IN_INT_GEN - 2
maxBitInt :: Int
maxBitInt = bitsInIntGen - 2

-- INTX2_MASK is used in calculation of the node numbers
intX2Mask :: Word32
intX2Mask = (1 `shiftL` maxBitInt) - 1

-- RUNUP keeps certain generators from looking too similar in the
-- first few words output
runup :: Int
runup = 2 * bitsInIntGen

-- GS0 gives a more "random" distribution of generators when the
-- user uses small integers as seeds
gs0 :: Word32
gs0 = 0x372f05ac

toomany :: String
toomany = "generator has branched maximum number of times;\nindependence of generators no longer guaranteed"

-- maxStreams :: Word32
-- maxStreams = 0x7fffffff

data Vstruct =
   Vstruct
   { vstruct_l :: !Int
   , vstruct_k :: !Int
   , vstruct_LSBS :: !Int  -- number of least significant bits that are 1
   , vstruct_first :: !Int -- the first seed whose LSB is 1
   }

-- XXX maybe an array or vector?
valid :: [Vstruct]
valid = [ Vstruct 1279 861 1 233
        , Vstruct 17 5 1 10
        , Vstruct 31 6 1 2
        , Vstruct 55 24 1 11
        , Vstruct 63 31 1 14
        , Vstruct 127 97 1 21
        , Vstruct 521 353 1 100
        , Vstruct 521 168 1 83
        , Vstruct 607 334 1 166
        , Vstruct 607 273 1 105
        , Vstruct 1279 418 1 208
        ]

-- count the number of 1 bits in an integer
bitcnt :: Bits a => a -> Word32
bitcnt x = go x 0
   where
   go y i
       | y == 0 = i
       | otherwise = go (y .&. (y - 1)) (i+1)

-- the register steps according to the primitive polynomial
-- (64,4,3,1,0); each call steps register 64 times
-- we use two words to represent the register to allow for integer
-- size of 32 bits
advanceReg :: PrimMonad m => Word32Vec m -> m ()
-- advanceReg :: Word32Vec IO -> IO ()
advanceReg regFill = do
   let mask = 0x1b :: Word32
   adv64 <- M.unsafeNew 8
   M.unsafeWrite adv64 0 0xb0000000
   M.unsafeWrite adv64 1 0x1b
   M.unsafeWrite adv64 2 0x60000000
   M.unsafeWrite adv64 3 0x2d
   M.unsafeWrite adv64 4 0xc0000000
   M.unsafeWrite adv64 5 0x5a
   M.unsafeWrite adv64 6 0x80000000
   M.unsafeWrite adv64 7 0xaf
   newFill <- M.unsafeNew 2
   M.unsafeWrite newFill 0 0
   M.unsafeWrite newFill 1 0
   let loop1 i temp
          | i < 0 = return ()
          | otherwise = do
               regFill0 <- M.unsafeRead regFill 0
               let twiddle y x = (x `shiftL` 1) .|. (1 .&. (bitcnt (y .&. temp)))
               _ <- modify newFill 0 $ twiddle regFill0
               regFill1 <- M.unsafeRead regFill 1
               _ <- modify newFill 1 $ twiddle regFill1
               loop1 (i - 1) (temp `shiftR` 1)
   loop1 (27::Int) (mask `shiftL` 27)
   let loop2 i
          | i >= 32 = return ()
          | otherwise = do
               regFill0 <- M.unsafeRead regFill 0
               let temp = bitcnt (regFill0 .&. (mask `shiftL` i))
               regFill1 <- M.unsafeRead regFill 1
               let temp' = temp `xor` (bitcnt (regFill1 .&. (mask `shiftR` (32 - i))))
               let twiddle y x = (x .|. ((1 .&. y) `shiftL` i))
               _ <- modify newFill 0 $ twiddle temp'
               adv64val0 <- M.unsafeRead adv64 ((i - 28) * 2)
               let temp = bitcnt (regFill0 .&. adv64val0)
               adv64val1 <- M.unsafeRead adv64 (((i - 28) * 2) + 1)
               let temp' = temp `xor` (bitcnt (regFill1 .&. adv64val1))
               _ <- modify newFill 1 $ twiddle temp'
               loop2 (i + 1)
   loop2 28
   newFill0 <- M.unsafeRead newFill 0
   M.unsafeWrite regFill 0 newFill0
   newFill1 <- M.unsafeRead newFill 1
   M.unsafeWrite regFill 1 newFill1

getFill :: PrimMonad m => Word32Vec m -> Word32Vec m -> Int -> Word32 -> m ()
-- getFill :: Word32Vec IO -> Word32Vec IO -> Int -> Word32 -> IO ()
getFill n r param seed = do
   -- XXX use something better than a list for valid
   let localVstruct = valid !! param
       length = vstruct_l localVstruct
   temp <- M.unsafeNew 2
   -- initialize the shift register with the node number XORed with
   -- the global seed
   -- fill the shift register with two copies of this number
   -- except when equal to zero
   n0 <- M.unsafeRead n 0
   let tempVal = n0 `xor` seed
   M.unsafeWrite temp 1 tempVal
   if tempVal == 0
      then M.unsafeWrite temp 0 gs0
      else M.unsafeWrite temp 0 tempVal
   -- advance the shift register some
   advanceReg temp
   advanceReg temp
   -- the first word in the generator is defined by the 31 LSBs of the
   -- node number
   M.unsafeWrite r 0 ((intMask .&. n0) `shiftL` 1)
   -- the generator is filled with the lower 31 bits of the shift
   -- register at each time, shifted up to make room for the bits
   -- defining the canonical form; the node number is XORed into
   -- the fill to make the generators unique
   forM_ [1 .. length-2] $ \i -> do
      advanceReg temp
      temp0 <- M.unsafeRead temp 0
      ni <- M.unsafeRead n i
      M.unsafeWrite r i ((intMask .&. (temp0 `xor` ni)) `shiftL` 1)
   M.unsafeWrite r (length-1) 0
   -- the canonical form for the LSB is instituted here
   let localStructFirst = vstruct_first localVstruct
       k = localStructFirst + vstruct_LSBS localVstruct
   forM_ [localStructFirst .. k - 1] $ \j ->
      modify r j (.|. 1)

-- updates index for next spawning
siDouble :: PrimMonad m => Word32Vec m -> Word32Vec m -> Int -> m ()
-- siDouble :: Word32Vec IO -> Word32Vec IO -> Int -> IO ()
siDouble a b length = do
   bLengthMinus2 <- M.unsafeRead b (length - 2)
   when ((bLengthMinus2 .&. (1 `shiftL` maxBitInt)) /= 0) $ do
      -- XXX this should raise a proper exception
      error ("Warning: siDouble " ++ toomany)
   M.unsafeWrite a (length - 2) ((intX2Mask .&. bLengthMinus2) `shiftL` 1)
   let initi = length - 3
   forM_ [initi, initi - 1 .. 0] $ \i -> do
      bi <- M.unsafeRead b i
      when ((bi .&. (1 `shiftL` maxBitInt)) /= 0) $
         modify a (i + 1) (+1) >> return ()
      M.unsafeWrite a i ((intX2Mask .&. bi) `shiftL` 1)

getNextElements :: PrimMonad m => Gen (PrimState m) -> m (Word32, Word32)
-- getNextElements :: Gen (PrimState IO) -> IO (Word32, Word32)
getNextElements gen = do
   let r0Local = gen_r0 gen
       r1Local = gen_r1 gen
       kv = gen_k gen
       lv = gen_l gen
   hptrWord32 <- M.unsafeRead r0Local lv
   let hptr = fromIntegral hptrWord32
   let lptr = wrapOver (hptr + kv) lv
   (newr0hptr, newr1hptr) <- addPrev hptr lptr r0Local r1Local
   let temp1 = (newr1hptr .&. complement 1) `xor` (newr0hptr `shiftR` 1)
   let hptrMinus1 = wrapUnder (hptr - 1) lv
   (newr0hptr, newr1hptr) <- addPrev hptrMinus1 (wrapUnder (lptr - 1) lv) r0Local r1Local
   let temp2 = (newr1hptr .&. complement 1) `xor` (newr0hptr `shiftR` 1)
   M.unsafeWrite r0Local lv (fromIntegral (wrapUnder (hptrMinus1 - 1) lv))
   return (temp1, temp2)
   where
   addPrev :: PrimMonad m => Int -> Int -> Word32Vec m -> Word32Vec m -> m (Word32, Word32)
   -- addPrev :: Int -> Int -> Word32Vec IO -> Word32Vec IO -> IO (Word32, Word32)
   addPrev hptr lptr r0 r1 = do
      -- INT_MOD_MASK causes arithmetic to be modular when integer size is
      -- different from generator modulus
      r0lptr <- M.unsafeRead r0 lptr
      newr0hptr <- modify r0 hptr $ \x -> intModMask .&. (x + r0lptr)
      r1lptr <- M.unsafeRead r1 lptr
      newr1hptr <- modify r1 hptr $ \x -> intModMask .&. (x + r1lptr)
      return (newr0hptr, newr1hptr)
   wrapOver :: Int -> Int -> Int
   wrapOver x lv
      | x >= lv = x - lv
      | otherwise = x
   wrapUnder :: Int -> Int -> Int
   wrapUnder x lv
      | x < 0 = lv - 1
      | otherwise = x

getRnWord32 :: PrimMonad m => Gen (PrimState m) -> m Word32
-- getRnInt :: Gen (PrimState IO) -> IO Word32
getRnWord32 gen = do
   (w1, _w2) <- getNextElements gen
   return (w1 `shiftR` 1)

getRnFlt :: PrimMonad m => Gen (PrimState m) -> m Float
-- getRnFlt :: Gen (PrimState IO) -> IO Float
getRnFlt gen = do
   (w1, _w2) <- getNextElements gen
   return (fromIntegral w1 * fltMult)

getRnDbl :: PrimMonad m => Gen (PrimState m) -> m Double
-- getRnDbl :: Gen (PrimState IO) -> IO Double
getRnDbl gen = do
   (w1, w2) <- getNextElements gen
   return ((fromIntegral w2 * fltMult + fromIntegral w1) * fltMult)

{-
getRnInt gen = do
   let r0Local = gen_r0 gen
       r1Local = gen_r1 gen
       kv = gen_k gen
       lv = gen_l gen
   hptrWord32 <- M.unsafeRead r0Local lv
   let hptr = fromIntegral hptrWord32
   let lptr = wrapOver (hptr + kv) lv
   (newr0hptr, newr1hptr) <- addPrev hptr lptr r0Local r1Local
   let newVal = (newr1hptr .&. complement 1) `xor` (newr0hptr `shiftR` 1)
   let hptrMinus1 = wrapUnder (hptr - 1) lv
   _ <- addPrev hptrMinus1 (wrapUnder (lptr - 1) lv) r0Local r1Local
   M.unsafeWrite r0Local lv (fromIntegral (wrapUnder (hptrMinus1 - 1) lv))
   return (newVal `shiftR` 1)
   where
   -- addPrev :: PrimMonad m => Int -> Int -> Word32Vec m -> Word32Vec m -> m (Word32, Word32)
   addPrev :: Int -> Int -> Word32Vec IO -> Word32Vec IO -> IO (Word32, Word32)
   addPrev hptr lptr r0 r1 = do
      -- INT_MOD_MASK causes arithmetic to be modular when integer size is
      -- different from generator modulus
      r0lptr <- M.unsafeRead r0 lptr
      newr0hptr <- modify r0 hptr $ \x -> intModMask .&. (x + r0lptr)
      r1lptr <- M.unsafeRead r1 lptr
      newr1hptr <- modify r1 hptr $ \x -> intModMask .&. (x + r1lptr)
      return (newr0hptr, newr1hptr)
   wrapOver :: Int -> Int -> Int
   wrapOver x lv
      | x >= lv = x - lv
      | otherwise = x
   wrapUnder :: Int -> Int -> Int
   wrapUnder x lv
      | x < 0 = lv - 1
      | otherwise = x
-}

-- initialize :: PrimMonad m => Int -> Int -> Word32 -> Word32Vec m -> Word32 -> m (Gen (PrimState m))
initialize :: PrimMonad m => Int -> Int -> Word32 -> Word32Vec m -> Word32 -> m (MutableArray (PrimState m) (Gen (PrimState m)))
-- initialize :: Int -> Word32 -> Word32Vec IO -> Word32 -> IO (Gen (PrimState IO))
initialize ngen param seed nstart initSeed = do
   let localVstruct = valid !! fromIntegral param
       length = vstruct_l localVstruct
   order <- M.unsafeNew ngen
   q <- newArray ngen undefined
   -- for (i=0;i<ngen_local;i++)
   forM_ [0 .. ngen - 1] $ \i -> do
       r0Vec <- M.unsafeNew (length + 1) -- extra element to store the hptr
       r1Vec <- M.unsafeNew length
       siVec <- M.unsafeNew (length - 1)
       let hptr = fromIntegral (length - 1)
       M.unsafeWrite r0Vec length hptr -- store hptr in the last element of r0
       nstart0 <- M.unsafeRead nstart 0
       let gen = Gen { gen_r0 = r0Vec
                     , gen_r1 = r1Vec
                     , gen_si = siVec
                     , gen_k = vstruct_k localVstruct
                     , gen_l = length
                     , gen_streamNumber = fromIntegral nstart0 -- This will be updated for gens at pos > 0
                     , gen_param = param
                     , gen_seed = seed
                     , gen_initSeed = initSeed
                     }
       writeArray q i gen
   q0 <- readArray q 0
   let q0_si = gen_si q0
       q0_r0 = gen_r0 q0
       q0_r1 = gen_r1 q0
   siDouble q0_si nstart length
   getFill q0_si q0_r0 param seed
   _ <- modify q0_si 0 (+1)
   getFill q0_si q0_r1 param seed
   M.unsafeWrite order 0 (0::Int)
   when (ngen > 1) $ do
       -- while(1)
       let whileLoop i = do
               let l = i
               -- for (k=0;k<l;k++)
               let forLoop i k
                      | k < l = do
                           orderk <- M.unsafeRead order k
                           qorderk <- readArray q orderk
                           let nindex = gen_si qorderk
                           qi <- readArray q i
                           nindex0 <- M.unsafeRead nindex 0
                           writeArray q i (qi { gen_streamNumber = fromIntegral nindex0 })
                           siDouble nindex nindex length
                           let qi_si = gen_si qi
                           -- for (j=0;j<length-1;j++)
                           forM_ [0..length-2] $ \j -> do
                               nindexj <- M.unsafeRead nindex j
                               M.unsafeWrite qi_si j nindexj
                           getFill qi_si (gen_r0 qi) param seed
                           _ <- modify qi_si 0 (+1)
                           getFill qi_si (gen_r1 qi) param seed
                           let next_i = i + 1
                           if (ngen == next_i)
                              then return next_i
                              else forLoop next_i (k + 1)
                      | otherwise = return i
               next_i <- forLoop i 0
               if (ngen == next_i)
                  then return ()
                  else do
                     -- for (k=l-1;k>0;k--)
                     forM_ [l-1,l-2 .. 1] $ \k -> do
                        M.unsafeWrite order (2*k+1) (l+k)
                        orderk <- M.unsafeRead order k
                        M.unsafeWrite order (2*k) orderk
                     M.unsafeWrite order 1 l
                     whileLoop next_i
       whileLoop 1
   -- for (i=ngen_local-1;i>=0;i--)
   let outerLoop i
          | i >= 0 = do
               qi <- readArray q i
               -- for (j=1;j<lv-1;j++)
               let innerLoop j
                      | j < (length - 1) = do
                           siJ <- M.unsafeRead (gen_si qi) j
                           if siJ /= 0
                              then return True
                              else innerLoop (j+1)
                      | otherwise = return False
               k <- innerLoop 1
               if k
                  then do
                     -- for (j=0;j<length*RUNUP;j++)
                     replicateM_ (length * runup) (getRnWord32 qi)
                     outerLoop (i - 1)
                else
                   return i
          | otherwise = return i
   i <- outerLoop (ngen - 1)
   -- while (i>=0)
   forM_ [i, i-1 .. 0] $ \i -> do
      qi <- readArray q i
      -- for (j=0;j<4*length;j++)
      replicateM_ (4 * length) (getRnWord32 qi)
   return q

initRng :: PrimMonad m => Int -> Int -> Word32 -> Int -> m (Gen (PrimState m))
-- initRng :: Int -> Int -> Word32 -> Int -> IO (Gen (PrimState IO))
initRng {- gen -} g {- total gen -} tg {- seed -} s {- parameter -} pa = do
   -- some error checking needs to go here
   -- Only 31 LSB of seed considered
   let seedMasked = s .&. 0x7fffffff
       localVstruct = valid !! fromIntegral pa
       length = vstruct_l localVstruct
   nstart <- M.unsafeNew (length - 1)
   M.unsafeWrite nstart 0 ((fromIntegral g) :: Word32)
   forM_ [1 .. length-2] $ \i ->
      M.unsafeWrite nstart i 0
   p <- initialize 1 pa (seedMasked `xor` gs0) nstart seedMasked
   p0 <- readArray p 0
   writeArray p 0 (p0 { gen_streamNumber = g })
   let siLocal = gen_si p0
   let whileLoop = do
          siLocal0 <- M.unsafeRead siLocal 0
          siLocal1 <- M.unsafeRead siLocal 1
          if (siLocal0 < fromIntegral tg && siLocal1 /= 0)
             then do
                siDouble siLocal siLocal length
                whileLoop
             else return ()
   whileLoop
   readArray p 0

spawnRng :: PrimMonad m => Gen (PrimState m) -> Int -> m [Gen (PrimState m)]
spawnRng gen nspawned = do
   let p = gen_si gen
   q <- initialize nspawned (gen_param gen) (gen_seed gen) p (gen_initSeed gen)
   let lval = gen_l gen
   siDouble p p lval
   buildList q 0 nspawned []
   where
   buildList array index size acc
      | index >= size = return $ reverse acc
      | otherwise = do
           next <- readArray array index
           buildList array (index+1) size (next:acc)

-- | A generator of an infinite (but periodic) sequence of pseudo random numbers.
data Gen s =
   Gen { gen_r0 :: M.MVector s Word32
       , gen_r1 :: M.MVector s Word32
       , gen_si :: M.MVector s Word32
       , gen_k :: !Int
       , gen_l :: !Int
       , gen_streamNumber :: !Int
       , gen_param :: !Int
       , gen_seed :: !Word32
       , gen_initSeed :: !Word32
       }
-- the size of the vector is K+1 elements. Elements at indices [0,K-1]
-- constitute the values of the lagTable. The element at index K is special,
-- it contains the value of the current cursor into the table.

-- | A shorter name for PRNG state in the IO monad.
type GenIO = Gen (PrimState IO)

-- | A shorter name for PRNG state in the ST monad.
type GenST s = Gen (PrimState (ST s))

-- | An exception type to indicate that the generator was not initialised correctly,
-- namely that the value of J was >= K.
data LFGException = InitLagException String
   deriving (Show, Typeable)

instance Exception LFGException

-- From here on down, the rest of the code is taken directly from the random-mwc
-- library. Thanks to Bryan O'Sullivan for writing the code. The code is
-- copyright 2009, 2010, 2011 to Bryan O'Sullivan, and released under the BSD3
-- license.

-- | Yield a random value, by transforming just one Word32 value.
{-
uniform1 :: PrimMonad m => (Word32 -> a) -> Gen (PrimState m) -> m a
uniform1 f gen = do
  (w1, _w2) <- getNextElements gen
  return $! f w1
{-# INLINE uniform1 #-}

-- | Yield a random value, by transforming two sequential Word32 values.
uniform2 :: PrimMonad m => (Word32 -> Word32 -> a) -> Gen (PrimState m) -> m a
uniform2 f gen = do
   (w1, w2) <- getNextElements gen
   return $! f w1 w2
{-# INLINE uniform2 #-}
-}

-- | The class of types for which we can generate uniformly
-- distributed random variates.
class M.Unbox a => Variate a where
    -- | Generate a single uniformly distributed random variate.  The
    -- range of values produced varies by type:
    --
    -- * For fixed-width integral types, the type's entire range is
    --   used.
    --
    -- * For floating point numbers, the range (0,1] is used. Zero is
    --   explicitly excluded, to allow variates to be used in
    --   statistical calculations that require non-zero values
    --   (e.g. uses of the 'log' function).
    --
    -- To generate a 'Float' variate with a range of [0,1), subtract
    -- 2**(-33).  To do the same with 'Double' variates, subtract
    -- 2**(-53).
    uniform :: (PrimMonad m) => Gen (PrimState m) -> m a
    -- | Generate single uniformly distributed random variable in a
    -- given range.
    --
    -- * For integral types inclusive range is used.
    --
    -- * For floating point numbers range (a,b] is used if one ignores
    --   rounding errors.
    uniformR :: (PrimMonad m) => (a,a) -> Gen (PrimState m) -> m a

instance Variate Int8 where
    -- uniform = uniform1 fromIntegral
    uniform gen = do { x <- getRnWord32 gen; return $ fromIntegral x }
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Int16 where
    -- uniform = uniform1 fromIntegral
    uniform gen = do { x <- getRnWord32 gen; return $ fromIntegral x }
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Int32 where
    -- uniform = uniform1 fromIntegral
    uniform gen = do { x <- getRnWord32 gen; return $ fromIntegral x }
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

{-
instance Variate Int64 where
    uniform = uniform2 wordsTo64Bit
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}
-}

instance Variate Word8 where
    -- uniform = uniform1 fromIntegral
    uniform gen = do { x <- getRnWord32 gen; return $ fromIntegral x }
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word16 where
    -- uniform = uniform1 fromIntegral
    uniform gen = do { x <- getRnWord32 gen; return $ fromIntegral x }
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word32 where
    -- uniform = uniform1 fromIntegral
    uniform gen = do { x <- getRnWord32 gen; return $ fromIntegral x }
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

{-
instance Variate Word64 where
    uniform = uniform2 wordsTo64Bit
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}
-}

instance Variate Bool where
    -- uniform = uniform1 wordToBool
    uniform gen = do { x <- getRnWord32 gen; return $ wordToBool x }
    uniformR (False,True)  g = uniform g
    uniformR (False,False) _ = return False
    uniformR (True,True)   _ = return True
    uniformR (True,False)  g = uniform g
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Float where
    -- uniform = uniform1 wordToFloat
    uniform = getRnFlt
    -- uniformR (x1,x2) = uniform1 (\w -> x1 + (x2-x1) * wordToFloat w)
    uniformR = error "uniformR undefined on float XXX fixme"
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Double where
    -- uniform = uniform2 wordsToDouble
    uniform = getRnDbl
    uniformR = error "uniformR undefined on double XXX fixme"
    -- uniformR (x1,x2) = uniform2 (\w1 w2 -> x1 + (x2-x1) * wordsToDouble w1 w2)
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

{-
instance Variate Int where
#if WORD_SIZE_IN_BITS < 64
    uniform = uniform1 fromIntegral
#else
    uniform = uniform2 wordsTo64Bit
#endif
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}
-}

{-
instance Variate Word where
#if WORD_SIZE_IN_BITS < 64
    uniform = uniform1 fromIntegral
#else
    uniform = uniform2 wordsTo64Bit
#endif
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}
-}

instance (Variate a, Variate b) => Variate (a,b) where
    uniform g = (,) `liftM` uniform g `ap` uniform g
    uniformR ((x1,y1),(x2,y2)) g = (,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance (Variate a, Variate b, Variate c) => Variate (a,b,c) where
    uniform g = (,,) `liftM` uniform g `ap` uniform g `ap` uniform g
    uniformR ((x1,y1,z1),(x2,y2,z2)) g =
      (,,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g `ap` uniformR (z1,z2) g
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance (Variate a, Variate b, Variate c, Variate d) => Variate (a,b,c,d) where
    uniform g = (,,,) `liftM` uniform g `ap` uniform g `ap` uniform g
                `ap` uniform g
    uniformR ((x1,y1,z1,t1),(x2,y2,z2,t2)) g =
      (,,,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g `ap`
                    uniformR (z1,z2) g `ap` uniformR (t1,t2) g
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

{-
wordsTo64Bit :: (Integral a) => Word32 -> Word32 -> a
wordsTo64Bit x y =
    fromIntegral ((fromIntegral x `shiftL` 32) .|. fromIntegral y :: Word64)
{-# INLINE wordsTo64Bit #-}
-}

wordToBool :: Word32 -> Bool
wordToBool i = (i .&. 1) /= 0
{-# INLINE wordToBool #-}

-- Type family for fixed size integrals. For signed data types it's
-- its unsigned couterpart with same size and for unsigned data types
-- it's same type
type family Unsigned a :: *

type instance Unsigned Int8  = Word8
type instance Unsigned Int16 = Word16
type instance Unsigned Int32 = Word32
type instance Unsigned Int64 = Word64
type instance Unsigned Int   = Word

type instance Unsigned Word8  = Word8
type instance Unsigned Word16 = Word16
type instance Unsigned Word32 = Word32
type instance Unsigned Word64 = Word64
type instance Unsigned Word   = Word

-- Subtract two numbers under assumption that x>=y and store result in
-- unsigned data type of same size
sub :: (Integral a, Integral (Unsigned a)) => a -> a -> Unsigned a
sub x y = fromIntegral x - fromIntegral y

add :: (Integral a, Integral (Unsigned a)) => a -> Unsigned a -> a
add m x = m + fromIntegral x

-- Generate uniform value in the range [0,n). Values must be
-- unsigned. Second parameter is random number generator
unsignedRange :: (PrimMonad m, Integral a, Bounded a) => a -> m a -> m a
unsignedRange n rnd = go
  where
    buckets = maxBound `div` n
    maxN    = buckets * n
    go = do x <- rnd
            if x < maxN then return (x `div` buckets)
                        else go
{-# INLINE unsignedRange #-}

-- Generate unformly distributed value in inclusive range.
uniformRange :: ( PrimMonad m
                , Integral a, Bounded a, Variate a
                , Integral (Unsigned a), Bounded (Unsigned a), Variate (Unsigned a))
             => (a,a) -> Gen (PrimState m) -> m a
uniformRange (x1,x2) g
  | x1 == minBound && x2 == maxBound = uniform g
  | otherwise                        = do x <- unsignedRange (sub x2 x1 + 1) (uniform g)
                                          return $! add x1 x
{-# INLINE uniformRange #-}

{-# INLINE modify #-}
modify :: (M.Unbox a, PrimMonad m) => M.MVector (PrimState m) a -> Int -> (a -> a) -> m a
modify vec i f = do
   old <- M.unsafeRead vec i
   let new = f old
   M.unsafeWrite vec i new
   return new
