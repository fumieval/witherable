# witherable

[![Discord](https://img.shields.io/discord/664807830116892674?color=%237095ec&label=Discord&style=plastic)](https://discord.gg/DG93Tgs)

witherable-class defines `Filterable` and `Witherable` class with instances.
witherable provides indexed variants, optic-style generalised withers and extra instances.

Why not just `fmap catMaybes . traverse f`? Because `wither` can be defined to traverse the structure once. Also, its parametrity ensures that filtering works in a sane way. For more details, see also https://twitter.com/viercc/status/1445571167591403524

Dependencies:

* base-orphans
* containers
* hashable
* transformers
* unordered-containers
* vector
