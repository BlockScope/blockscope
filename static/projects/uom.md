---
title: 'Units of Measure'
subtitle: We're not comparing apples and oranges, are we?
---
Professionally, I develop software applications. When I most needed better types
for units I didn't know that types could help with checking unit conformance and
conversion.

At the time I joined a project[^1] to commercialise a potato crop model it was
coded in Borland Builder C++ and built one executable. This Windows GUI [^2]
desktop app, agronomists could use to setup and run the model. It had pickers
for input files and a presentation layer for the results of the model run.  I
didn't need the GUI elements as we would be delivering these via the web in the
commercial product.

The model itself was not a lot of code and within a couple of days I was able to
port it to standard C++ and then to C#. There were comments in places that
mentioned units, mostly on numeric properties of classes.  Even with those
comments, as a developer without prior experience in this domain, I couldn't
step through the code and know what a sensible range would be for any variable
I'd drag into the watch pane. When I ran the model I was getting different
outputs than expected.

Familiar with the model, the research programmer at the institute had a good
working knowledge of sensible ranges and pinpointed the problem quickly.  Turned
out to be a unit conversion error. A needle in a haystack problem for me, not a
domain insider.

Later on, we used the [unified
code](https://en.wikipedia.org/wiki/Unified_Code_for_Units_of_Measure) for units
as XML. That was some help but not type level help.

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

[^1]: Crop Logic, a now defunct startup spun out of a research institute.
[^2]: A graphical user interface.