0.4.2
-------

* Supported GHC 9.2
* Improved the instances for `vector`

0.4.1
-------
* Added `ordNubBy`, `hashNubBy`, `ordNubByOf`, and `hashNubByOf`.
* Use `alterF` for nub-function implementations
* Implement `witherM` in `Witherable Vector` instance.
* Mark modules as Trustworthy
* `ordNub` and `hashNub` are productive, start to produce results immediately and work for infinite lists.

0.4
-------
* `FilterableWithIndex` and `WitherableWithIndex` are now subclasses of the ones from [indexed-traversable](https://hackage.haskell.org/package/indexed-traversable)
* Removed the orphan instances for `MonoidalMap`

0.3.5
-------

* Make `wither` and `witherM` methods of `Witherable []` instance
  good consumers for list fusion.
* Added instances for `Reverse`, `Backwards`, `ZipList`, and types from `GHC.Generics`
* Added `Wither`, `WitherLike`, `Wither'` and `WitherLike'`, deprecating `Filter` and the variants
* Moved `Filterable` and `Witherable` into a separate package, `witherable-class`

0.3.4
-------
* Exported `WrappedFoldable`

0.3.3
-------

* Added `FilterableWithIndex` and `WitherableWithIndex`.
* Added `WrappedFoldable`

0.3.2
----------

* Added `Filterable (MonoidalMap k)` and `Witherable (MonoidalMap k)`

0.3.1
-------
* Added `(<$?>)` as an alias for `mapMaybe`, with fixity matching `(<$>)`.
* Added `(<&?>) = flip (<$?>)`, with fixity matching `(<&>)`.

0.3
-------
* Added `(Filterable f, Filterable g) => Filterable (Product f g)`
* Added `(Witherable f, Witherable g) => Witherable (Product f g)`
* Added `(Filterable f, Filterable g) => Filterable (Sum f g)`
* Added `(Witherable f, Witherable g) => Witherable (Sum f g)`
* Added `Filterable f => Filterable (IdentityT f)`
* Added `Witherable f => Witherable (IdentityT f)`
* Switched from strict `HashMap` operations to lazy ones. This
  matches the behavior of the rest of the instances.
* Changed the definition of `witherM`

0.2
-------
* Added `Traversable t => Witherable (MaybeT t)`
* New class: `Filterable`
  * `Witherable t` is equivalent to `(Traversable t, Filterable t)`
* Removed `Chipped`

0.1.3.3
-------
* Added `forMaybeOf` and `forMaybe`

0.1.3.2
-------
* Exported `witherM`, `blightM`
* Fixed the default definition of `catMaybes`

0.1.3
-------
* Now `witherable` depends on `base-orphans` to prevent a tragedy of duplicate orphans
* Added generalized combinators according to the `lens` convention

0.1.2.3
-------
* Added `ordNub`, `hashNub`
* Data.Witherable is now Trustworthy

0.1.2.2
-------
* Added `Chipped`
