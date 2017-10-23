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
