---
title: How would this look in idiomatic F#?
tags: fsharp, conversion, xamarin
---
I've been watching the play for apps from the sidelines. Had I'd missed the
boat by not getting in early with the first wave of farting and beer swilling
apps for mobile? Was the development experience raw and retrograde, like back
to the '90s before virtual machines with many build targets and ```#pragma```
hell?  [Xamarin](http://xamarin.com/) helps ease that cross-platform pain? With
the recent release of F# 3.0 came another open source drop of the compiler and
tools and a language binding for F# 3.0 has been added to [Xamarin
Studio](http://xamarin.com/studio).

<blockquote class="twitter-tweet"><p>Beloved F# istas, how would you do this in
idiomatic F# to be shorter, ex: the error handling: <a
href="https://t.co/bGuDdcyxPV"
title="https://gist.github.com/lobrien/5250556">gist.github.com/lobrien/5250556</a></p>&mdash;
Miguel de Icaza (@migueldeicaza) <a
href="https://twitter.com/migueldeicaza/status/316729282124709889">March 27,
2013</a></blockquote> <script async src="//platform.twitter.com/widgets.js"
charset="utf-8"></script>

In response to this tweet, I set about converting the C# in that gist to F#,
figuring it would be a good way to take the Xamarin tooling for a test drive.
To get started, I created a new F# project in Visual Studio but changed the
default references to the mono assemblies. After a transcription from C# to F#,
the code changes I made were ...

* Put the moving bits inside an auto-opening private module. This is something
  like an anonymous namespace in C++, a place to put stuff that is local to the
  source file but otherwise hidden. What was left to the class members was to
  unwrap a ```Choice<_, _>```, possibly fire an alert off and return an
  ```Option<_>``` ...

```fsharp
        [<AutoOpen>]
        module private __ =
            let initializeSession f =
                // the real work gets done here

        type VideoCapture(labelledView) = 
            inherit AVCaptureVideoDataOutputSampleBufferDelegate()
            // ...
            member x.InitializeSession () =
                match initializeSession x with
                | Choice1Of2(s) -> Some(s)
                | Choice2Of2(m) -> alert(m); None
```

* I stopped naming things I didn't care about using underscore, the discard
  binding from pattern matches. Note that I cannot use a single underscore for
  the this binding of a method so I use two.

```fsharp
        override __.FinishedLaunching(_, _) = ...
```

* Replaced nulls with options and choices.
* Dropped the use of new when constructing object except when the class
  implements IDisposable.
* Collected related fields into records going with immutability where possible.

```fsharp
        // From this ...
        type VideoCapture(imgView, label) = 
            inherit AVCaptureVideoDataOutputSampleBufferDelegate()
            member val ImageView : UIImageView = null with get, set
            member val InfoLabel : UILabel = null with get, set

        // To this ...
        type LabelledView = {Label : UILabel; View : UIImageView}
        type VideoCapture(labelledView) = 
            inherit AVCaptureVideoDataOutputSampleBufferDelegate()
```

* Used pattern matching to deconstruct records.

```fsharp
        match labelledView with
        | {Label = l; View = v} ->
```

* Dropped the `Maybe-` prefix on methods that were returning null but were now
  returning ```Option<_>```.
* I preferred to use ```Ref<_>``` for the few remaining mutations of fields.
* Encoded the same information in fewer fields by seeing that recording was
  tied to the existence of the capture.

```fsharp
        type VideoCaptureController(viewColor, title) =
            inherit UIViewController()
            // from this ...
            member val recording =
                false with get, set
            member val videoCapture : VideoCapture =
                new VideoCapture(cv.LabelledView) with get, set

            // to this ...
            let capture : Ref<VideoCapture option> = ref None
```

The [complete solution](https://github.com/philderbeast/XamarinVideoCapture)
and [gist](https://gist.github.com/philderbeast/5253070) are up at github.
I coded this blind, aiming only to get it compiling and don't know if it runs.
