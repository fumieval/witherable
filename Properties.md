## Empty values

As a consequence of the `Filterable` laws, applying `mapMaybe` to an
"empty" value does nothing. More precisely:

### Definition

Suppose that `f` is a `Functor`, `xs :: f a` and that for all `b :: Type` and
all `f, g :: a -> b`,

```haskell
fmap f xs = fmap g xs
```

We will call `xs` _empty_ because it does not actually contain or produce any `a`s.

### Theorem 1. `mapMaybe` with an empty value

Let `xs :: f a` be empty.

Then for all `c :: Type` and all `h, i :: a -> Maybe c`,

```haskell
mapMaybe h xs = mapMaybe i xs
```

#### Proof

Applying the preservation law to the assumption, we see that for all
`f, g :: a -> b`,

```haskell
mapMaybe (Just . f) xs = mapMaybe (Just . g) xs
```

Therefore (using `b ~ Maybe c`), for all `c :: Type` and all `h, i :: a -> Maybe c`,

```haskell
mapMaybe (Just . h) $ xs = mapMaybe id . mapMaybe (Just . i) $ xs
```

Composing on both sides,

```haskell
mapMaybe id . mapMaybe (Just . h) $ xs = mapMaybe id . mapMaybe (Just . i) $ xs
```

By the composition law,

```haskell
mapMaybe id . mapMaybe (Just . h) $ xs
  = mapMaybe (id <=< Just . h) xs
  = mapMaybe (\x -> Just (h x) >>= id) xs
  = mapMaybe h xs
```

and similarly for `i`.

Thus

```haskell
mapMaybe h xs = mapMaybe i xs
```

### Theorem 2. `wither` with an empty value

Suppose `xs :: f a` is empty, `c :: Type`, and `f, g :: a -> f (Maybe b)`.
Then

```haskell
wither f xs = wither g xs
```

#### Proof

First, we show that `wither (fmap Just . f) xs = wither (fmap Just . g) xs`:

```haskell
wither (fmap Just . f) xs
  = traverse f xs          -- Preservation
  = sequenceA (fmap f xs)  -- Default definition of traverse
  = sequenceA (fmap g xs)  -- xs is empty
  = traverse g xs
  = wither (fmap Just . g) xs
```

Applying the composition law,

```haskell
wither (Compose . fmap (wither Identity) . fmap Just . f) xs =
wither (Compose . fmap (wither Identity) . fmap Just . g) xs
```

By the second functor law,

```haskell
wither (Compose . fmap (wither Identity . Just) . f) xs =
wither (Compose . fmap (wither Identity . Just) . g) xs
```

Inlining the definition of `wither` for `Maybe` and reducing,

```haskell
wither (Compose . fmap Identity . f) xs =
wither (Compose . fmap Identity . g) xs
```

Since `Compose . fmap Identity` is an applicative transformation (see Lemma 1
below), we can apply the naturality law:

```haskell
Compose . fmap Identity $ wither f xs =
Compose . fmap Identity $ wither g xs
```

`fmap runIdentity . getCompose . Compose . fmap Identity = id`,
so `wither f xs = wither g xs`.

## Alternative notions of emptiness

While the definition above seems to be the only sensible way to
express the idea that an arbitrary `Functor` or `Filterable` is
empty, there are other ways to express that concept for a `Traversable`
or `Witherable`.

### Theorem 3

A value `xs :: t a` is empty if and only if the following holds:

For all `b`, all `Applicative m`, and all `f, g :: a -> m b`,

```haskell
traverse f xs = traverse g xs
```

#### Proof

Suppose `xs :: t a` is empty, and `b`, `m`, `f`, and `g` are as described.
Then by Theorem 2,

```haskell
wither (fmap Just . f) xs = wither (fmap Just . g) xs
```

By preservation,

```haskell
traverse f xs = traverse g xs
```

Conversely, suppose that for all `b`, `Applicative m`, and `h, i :: a -> m b`,
`traverse h xs = traverse i xs`. Let `f, g :: a -> c`. Then choosing `m ~ Identity`,
`runIdentity $ traverse (Identity . f) xs = runIdentity $ traverse (Identity . g) xs`.
By the `traverse/fmap` law, `fmap f xs = fmap g xs`. As this holds for arbitrary
`c`, `f`, and `g`, `xs` is empty.
