module Hs2.Types.Loc where

import GHC.Generics
import Data.Data
import Data.Bifunctor

data SrcPos = SrcPos
  { srcPosLine :: Int
  , srcPosCol  :: Int
  } deriving stock (Eq, Ord, Show, Data, Generic)


data SrcSpan = SrcSpan FilePath SrcPos SrcPos
  deriving stock (Eq, Ord, Show, Data, Generic)

data Loc a meta = Loc
  { locSpan :: SrcSpan
  , locMeta :: meta
  , locVal  :: a
  } deriving (Eq, Ord, Show, Functor, Traversable, Foldable, Data)

instance Bifunctor Loc where
  first f (Loc span meta a) = Loc span meta (f a)
  second = fmap

class GBimap a b c d i o where
  gbimap :: (a -> b) -> (c -> d) -> i x -> o x

instance GBimap a b c d i o => GBimap a b c d (M1 _1 _2 i) (M1 _3 _4 o) where
  gbimap f g = M1 . gbimap f g . unM1

instance GBimap a b c d U1 U1 where
  gbimap _ _ U1 = U1

instance (GBimap a b c d fi fo, GBimap a b c d gi go) => GBimap a b c d (fi :*: gi) (fo :*: go) where
  gbimap f g (a :*: b) = gbimap f g a :*: gbimap f g b

instance (GBimap a b c d fi fo, GBimap a b c d gi go) => GBimap a b c d (fi :+: gi) (fo :+: go) where
  gbimap f g (L1 a) = L1 $ gbimap f g a
  gbimap f g (R1 b) = R1 $ gbimap f g b

instance {-# INCOHERENT #-} GBimap a b c d (K1 _1 a) (K1 _1 b) where
  gbimap f _ (K1 a) = K1 $ f a

instance {-# INCOHERENT #-} GBimap a b c d (K1 _1 c) (K1 _1 d) where
  gbimap _ g (K1 b) = K1 $ g b

instance {-# INCOHERENT #-} Bifunctor p => GBimap a b c d (K1 _1 (Loc c (p a c))) (K1 _1 (Loc d (p b d))) where
  gbimap f g (K1 loc) = K1 $ bimap g (bimap f g) loc

instance {-# INCOHERENT #-} Bifunctor p => GBimap a b c d (K1 _1 (p a c)) (K1 _1 (p b d)) where
  gbimap f g (K1 loc) = K1 $ bimap f g loc

instance {-# INCOHERENT #-} GBimap a b c d (K1 _1 x) (K1 _1 x) where
  gbimap _ _ (K1 x) = K1 x

instance (Functor q, GBimap a b c d (K1 _1 i) (K1 _1 o))
    => GBimap a b c d (K1 _1 (q i)) (K1 _1 (q o)) where
  gbimap f g (K1 loc) =
    K1 $ fmap (unK1 . gbimap @_ @_ @_ @_ @(K1 _1 i) @(K1 _1 o) f g . K1) loc

glocmap
  :: (GBimap a1 b c1 d (Rep a2) (Rep c2), Generic c2, Generic a2) =>
     (a1 -> b) -> (c1 -> d) -> a2 -> c2
glocmap f g = to . gbimap f g . from


