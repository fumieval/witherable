# Witherable laws

This document describes the `Witherable` laws
with more detail than included in the haddock.

A `Functor t` is `Witherable` if `t` equips `wither` function below,
and `wither` satisfy these three properties.

```haskell
wither :: Applicative f => (a -> f (Maybe b)) -> t a -> f (t b)
```

* **Naturality**

  For every applicative transformation `tr`,
  
  ```haskell
  tr . wither f = wither (tr . f)
  ```

* **Identity**

  ```haskell
  wither (Identity . Just) = Identity
  ```

* **Composition**

  ```haskell
  wither (Compose . fmap (witherMaybe g) . f) = Compose . fmap (wither g) . wither f
  ```
  
  Here, `witherMaybe` is the `wither` method of `Witherable Maybe` instance,
  which is defined as
  
  ```haskell
  witherMaybe :: Applicative f => (a -> f (Maybe b)) -> Maybe a -> f (Maybe b)
  witherMaybe f = fmap join . traverse f
  ```
  
  Or more concretely
  
  ```haskell
  witherMaybe _ Nothing = pure Nothing
  witherMaybe f (Just a) = f a
  ```

These facts follow from the laws.

* `Witherable t` implies `Traversable t` by the following definition of `traverse`.

  ```haskell
  traverse :: (Witherable t, Applicative f) => (a -> f b) -> t a -> f (t b)
  traverse f = wither (fmap Just . f)
  ```

  Check `Traversable` laws.

  * **Traversable-Naturality**

    ```haskell
    -- tr is an Applicative transformation
    tr . traverse f
     = tr . wither (fmap Just . f)
     = wither (tr . fmap Just . f)
     = wither (fmap Just . tr . f)
     = traverse (tr . f)
    ```

  * **Traversable-Identity**
    
    ```haskell
    traverse Identity
     = wither (fmap Just . Identity)
     = wither (Identity . Just)
       -- Identity law
     = Identity
    ```

  * **Traversable-Composition**
  
    ```haskell
    Compose . fmap (traverse g) . traverse f
     = Compose . fmap (wither (fmap Just . g)) . wither (fmap Just . f)
     = wither (Compose . fmap (witherMaybe (fmap Just . g)) . fmap Just . f)
     = wither (Compose . fmap (witherMaybe (fmap Just . g) . Just) . f)
     = wither (Compose . fmap (fmap join . traverse (fmap Just . g) . Just) . f)
     = wither (Compose . fmap (fmap join . fmap Just . fmap Just . g) . f)
     = wither (Compose . fmap (fmap Just . g) . f)
     = wither (fmap Just . Compose . fmap g . f)
     = traverse (Compose . fmap g . f)
    ```

  Thus it's reasonable to require `Traversable` as a superclass of
  `Witherable`, where `traverse` matches the above default definition.
  This requirement is named *conservation* law.
  
  * **Conservation** (relates `Witherable` and `Traversable`)
  
    ```haskell
    wither (fmap Just . f) = traverse f
    ```

* `Witherable t` implies `Filterable t` by the following definition of `mapMaybe`.

  ```haskell
  mapMaybe :: Witherable t => (a -> Maybe b) -> t a -> t b
  mapMaybe f = runIdentity . wither (Identity . f)
  ```
  
  `mapMaybe` satisfies the `Filterable` laws.
  
  * **Filterable-Conservation**
  
    ```haskell
    -- Shorthand
    I = Identity
    unI = runIdentity
    
    mapMaybe (Just . f)
     = unI . wither (I . Just . f)
       -- By parametricity
     = unI . wither (I . Just) . fmap f
       -- Identity law
     = fmap f
    ```

  * **Filterable-Composition**
  
    ```haskell
    mapMaybe f . mapMaybe g
     = unI . wither (I . f) . unI . wither (I . g)
     = unI . unI . fmap (wither (I . f)) . wither (I . g)
     = unI . unI . getCompose . Compose . fmap (wither (I . f)) . wither (I . g)
     = unI . unI . getCompose . wither (Compose . fmap (witherMaybe (I . f)) . I . g)
       -- (unI . getCompose :: Compose Identity Identity ~> Identity)
       -- is Applicative transformation
     = unI . wither (unI . getCompose . Compose . fmap (witherMaybe (I . f)) . I . g)
     = unI . wither (unI . fmap (witherMaybe (I . f)) . I . g)
     = unI . wither (unI . I . witherMaybe (I . f) . g)
     = unI . wither (witherMaybe (I . f) . g)
     = unI . wither (fmap join . traverse (I . f) . g)
     = unI . wither (fmap join . I . fmap f . g)
     = unI . wither (I . (f <=< g))
     = mapMaybe (f <=< g)
    ```
  
  Thus it's reasonable to require `Filterable` as a superclass of
  `Witherable`, where `mapMaybe` matches the above default definition.
  This requirement is named *pure filter* law.
  
  * **Pure filter**
  
    ```haskell
    runIdentity (wither (Identity . f)) = mapMaybe f
    ```
    
    or more concisely,
    
    ```haskell
    wither (Identity . f) = Identity . mapMaybe f
    ```
  
* Because `gen (Identity a) = pure a` is an applicative transformation `Identity ~> g`
  for any `Applicative g`, combining **Pure Filter** and **Naturality** you can conclude
  
  ```
  wither (pure . f) = pure . mapMaybe f
  ```

From the above laws, it can be proven that lawful `wither` is unique given
`traverse` and `mapMaybe`:

* **Canonicity**

  ```haskell
  wither f = fmap catMaybes . traverse f
  ```

_Proof._

Note that `catMaybes = mapMaybe id`.

```haskell
Compose . fmap Identity . fmap catMaybes . traverse f
 = Compose . fmap (Identity . catMaybes) . traverse f
 = Compose . fmap (wither Identity) . wither (fmap Just . f)
 = wither (Compose . fmap (witherMaybe Identity) . (fmap Just . f))
 = wither (Compose . fmap (witherMaybe Identity . Just) . f)
 = wither (Compose . fmap Identity . f)
   -- (Compose . fmap Identity :: g ~> Compose g Identity)
   -- is an applicative transfromation
 = Compose . fmap Identity . wither f
```

Because `(Compose . fmap Identity)` is an isomorphism, we can conclude
the equation we wanted to show.

```haskell
fmap catMaybes . traverse f = wither f
```

# Witherable laws, alternative formulation

These two laws are equivalent to the above set of laws.

* **Canonicity**

  ```haskell
  wither f = fmap catMaybes . traverse f
  ```

* **Distributivity**

  ```haskell
  traverse f . catMaybes = fmap catMaybes . traverse (traverse f)
  ```

It's already showed that the original laws imply **Canonicity**.
They imply **Distributivity** too.

_Proof._

```haskell
Compose . Identity . traverse f . catMaybes
 = Compose . fmap (traverse f) . Identity . catMaybes
 = Compose . fmap (wither (fmap Just . f)) . wither Identity
 = wither (Compose . fmap (witherMaybe (fmap Just . f)) . Identity)
 = wither (Compose . Identity . witherMaybe (fmap Just . f))
 = wither (Compose . Identity . traverse f)
   -- (Compose . Identity) is an applicative transformation
 = Compose . Identity . wither (traverse f)
   -- Canonicity is already proven equation
 = Compose . Identity . fmap catMaybes . traverse (traverse f)

And (Compose . Identity) is isomorphism. We can conclude

traverse f . catMaybes = fmap catMaybes . traverse (traverse f)
```

Then let's do the other direction. **Canonicity** + **Distributivity**,
along with `Filterable` and `Traversable` laws,
prove all three of `Witherable` laws + **Conservation** + **Pure filter**.

_Proof._
```haskell
-- Naturality
n . wither f
   -- Canonicity
 = n . fmap catMaybes . traverse f
   -- n is natural transformation
 = fmap catMaybes . n . traverse f
   -- Naturality of traverse
 = fmap catMaybes . traverse (n . f)
   -- Canonicity
 = wither (n . f)

-- Conservation
wither (fmap Just . f)
   -- Canonicity
 = fmap catMaybes . traverse (fmap Just . f)
 = fmap catMaybes . fmap (fmap Just) . traverse f
 = fmap (catMaybes . fmap Just) . traverse f
 = fmap (mapMaybe Just) . traverse f
   -- Filterable law
 = traverse f

-- Pure filter
wither (Identity . f)
   -- Canonicity
 = fmap catMaybes . traverse (Identity . f)
   -- Traversable law, Identity
 = fmap catMaybes . Identity . fmap f
 = Identity . catMaybes . fmap f
 = Identity . mapMaybe f

-- Identity
wither (Identity . Just)
   -- use Pre filter
 = Identity . mapMaybe Just
   -- Filterable law
 = Identity

-- Composition
Compose . fmap (wither g) . wither f
   -- Canonicity
 = Compose . fmap (fmap catMaybes . traverse g) . fmap catMaybes . traverse f
 = Compose . fmap (fmap catMaybes) . fmap (traverse g . catMaybes) . traverse f
   -- Distributivity
 = Compose . fmap (fmap catMaybes) . fmap (fmap catMaybes . traverse (traverse g)) . traverse f
 = Compose . fmap (fmap catMaybes) . fmap (fmap catMaybes) . fmap (traverse (traverse g)) . traverse f
   -- The definition of fmap for Compose
 = fmap (catMaybes . catMaybes) . Compose . fmap (traverse g) . traverse f
   -- Traversable law, composition
 = fmap (catMaybes . catMaybes) . traverse (Compose . fmap (traverse g) . f)
   -- Filterable law, composition
 = fmap (catMaybes . fmap join) . traverse (Compose . fmap (traverse g) . f)
 = fmap catMaybes . fmap (fmap join) . traverse (Compose . fmap (traverse g) . f)
 = fmap catMaybes . traverse (fmap join . Compose . fmap (traverse g) . f)
   -- Canonicity
 = wither (Compose . fmap (fmap join . traverse g) . f)
   -- Definition of witherMaybe
 = wither (Compose . fmap (witherMaybe g) . f)
```
