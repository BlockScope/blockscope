---
title: 'Units of Measure'
subtitle: We're not comparing apples and oranges, are we?
---
I've been quite interested in units of measure in programming languages since I
worked on a 3-year project to commercialise a potato crop model. The model was
in NZ customary units; kgs, hectares and litres. We stored the data in SI units
but we also had to display in the user's choice of units, such as acre and
gallon and even some unusual units such as hundredweight. We used the [unified code](https://en.wikipedia.org/wiki/Unified_Code_for_Units_of_Measure) for units of measure as XML.

```xml
<?xml version="1.0" encoding="ascii"?>
<root>
   <prefix Code="k" CODE="K">
      <name>kilo</name>
      <printSymbol>k</printSymbol>
      <value value="1e3">1 &#215; 10<sup>3</sup>
      </value>
   </prefix>
   ...
   <base-unit Code="m" CODE="M" dim="L">
      <name>meter</name>
      <printSymbol>m</printSymbol>
      <property>length</property>
   </base-unit>
   <base-unit Code="s" CODE="S" dim="T">
      <name>second</name>
      <printSymbol>s</printSymbol>
      <property>time</property>
   </base-unit>
   <base-unit Code="g" CODE="G" dim="M">
      <name>gram</name>
      <printSymbol>g</printSymbol>
      <property>mass</property>
   </base-unit>
   ...
   <unit Code="Hz" CODE="HZ" isMetric="yes" class="si">
      <name>Herz</name>
      <printSymbol>Hz</printSymbol>
      <property>frequency</property>
      <value Unit="s-1" UNIT="S-1" value="1">1</value>
   </unit>
   <unit Code="N" CODE="N" isMetric="yes" class="si">
      <name>Newton</name>
      <printSymbol>N</printSymbol>
      <property>force</property>
      <value Unit="kg.m/s2" UNIT="KG.M/S2" value="1">1</value>
   </unit>
```

In C# we parsed this and added some conversion functions. There has to be a
better way, right? Well there is. F# has units of measure built in. They're
typed checked and erased. I've contributed to type checker plugins that help
GHC, the Haskell compiler, with units of measure.

* `adamgundry`[**/units-parser**](https://github.com/adamgundry/units-parser)  
[Bump](https://github.com/adamgundry/units-parser/commit/9db2652bfbeea5d69f590ce15c171d7b9801bb60)
expected test output for later ghc versions.
* `adamgundry`[**/uom-plugin**](http://hackage.haskell.org/package/uom-plugin)  
A compiler plugin for units of measure that
I [contribute](https://github.com/adamgundry/uom-plugin/graphs/contributors)
to and use in flare-timing.
* `bgamari`[**/the-thoralf-plugin**](https://cs.brynmawr.edu/~rae/papers/2018/thoralf/thoralf.pdf)  
After seeing a presentation on this compiler plugin at the Haskell symposium
2018,
I [fixed](https://github.com/bgamari/the-thoralf-plugin/commits?author=philderbeast)
some build warnings and got it compiling with ghc-8.6.1.
* `BlockScope`[**/plugins-for-blobs**](https://github.com/BlockScope/plugins-for-blobs)  

    > 1 blob is equal to 1 lbf⋅s2/in, or 12 [slugs][slug].

    This is my attempt to refactor the [thoralf-plugin][thoralf-plugin] and the
    [uom-plugin][uom-plugin] so that they share something in common and so that the former reaches parity with the later in units of measure.

  [slug]: https://en.wikipedia.org/wiki/Slug_(unit)
  [uom-plugin]: https://github.com/adamgundry/uom-plugin
  [thoralf-plugin]: https://github.com/bgamari/the-thoralf-plugin
  [ghc-tcplugins-extra]: https://github.com/BlockScope/ghc-tcplugins-extra
  [units-parser]: https://github.com/adamgundry/units-parser