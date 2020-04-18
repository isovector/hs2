module Hs2.Types.Loc where

import GHC.Generics
import Data.Data
import Data.Bifunctor
import Data.Bitraversable
import Data.Bifoldable

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
  } deriving (Eq, Ord, Show, Functor, Traversable, Foldable, Data, Generic)

instance Bifunctor Loc where
  first f (Loc span meta a) = Loc span meta (f a)
  second = fmap

instance Bifoldable Loc where
  bifoldMap = bifoldMapDefault

instance Bitraversable Loc where
  bitraverse = bitrav


data DLoc a meta = DLoc
  { dlocSpan :: SrcSpan
  , dlocVal  :: a
  } deriving (Eq, Ord, Show, Functor, Traversable, Foldable, Data, Generic)

instance Bifunctor DLoc where
  first f (DLoc span a) = DLoc span (f a)
  second = fmap

instance Bifoldable DLoc where
  bifoldMap = bifoldMapDefault

instance Bitraversable DLoc where
  bitraverse = bitrav

class GBitraversable a b c d i o where
  gbitraverse :: Applicative f => (a -> f b) -> (c -> f d) -> i x -> f (o x)

instance GBitraversable a b c d i o => GBitraversable a b c d (M1 _1 _2 i) (M1 _3 _4 o) where
  gbitraverse f g = fmap M1 . gbitraverse f g . unM1

instance GBitraversable a b c d U1 U1 where
  gbitraverse _ _ U1 = pure U1

instance (GBitraversable a b c d fi fo, GBitraversable a b c d gi go) => GBitraversable a b c d (fi :*: gi) (fo :*: go) where
  gbitraverse f g (a :*: b) = (:*:) <$> gbitraverse f g a <*> gbitraverse f g b

instance (GBitraversable a b c d fi fo, GBitraversable a b c d gi go) => GBitraversable a b c d (fi :+: gi) (fo :+: go) where
  gbitraverse f g (L1 a) = fmap L1 $ gbitraverse f g a
  gbitraverse f g (R1 b) = fmap R1 $ gbitraverse f g b

instance {-# INCOHERENT #-} GBitraversable a b c d (K1 _1 a) (K1 _1 b) where
  gbitraverse f _ (K1 a) = fmap K1 $ f a

instance {-# INCOHERENT #-} GBitraversable a b c d (K1 _1 c) (K1 _1 d) where
  gbitraverse _ g (K1 b) = fmap K1 $ g b

instance {-# INCOHERENT #-} Bitraversable p => GBitraversable a b c d (K1 _1 (Loc c (p a c))) (K1 _1 (Loc d (p b d))) where
  gbitraverse f g (K1 loc) = K1 <$> bitraverse g (bitraverse f g) loc

instance {-# INCOHERENT #-} Bitraversable p => GBitraversable a b c d (K1 _1 (DLoc c (p a c))) (K1 _1 (DLoc d (p b d))) where
  gbitraverse f g (K1 loc) = fmap K1 $ bitraverse g (bitraverse f g) loc

instance {-# INCOHERENT #-} Bitraversable p => GBitraversable a b c d (K1 _1 (p a c)) (K1 _1 (p b d)) where
  gbitraverse f g (K1 loc) = fmap K1 $ bitraverse f g loc

instance {-# INCOHERENT #-} GBitraversable a b c d (K1 _1 x) (K1 _1 x) where
  gbitraverse _ _ (K1 x) = pure $ K1 x

instance (Traversable q, GBitraversable a b c d (K1 _1 i) (K1 _1 o))
    => GBitraversable a b c d (K1 _1 (q i)) (K1 _1 (q o)) where
  gbitraverse f g (K1 loc) =
    fmap K1 $ traverse (fmap unK1 . gbitraverse @_ @_ @_ @_ @(K1 _1 i) @(K1 _1 o) f g . K1) loc

bitrav
  :: (GBitraversable a1 b1 c d (Rep a2) (Rep b2), Applicative f, Generic b2, Generic a2)
  => (a1 -> f b1)
  -> (c -> f d)
  -> a2
  -> f b2
bitrav f g = fmap to . gbitraverse f g . from

