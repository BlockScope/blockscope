# Block Scope

### Styling

```
> yarn run pack
yarn run v1.17.3
$ webpack
Hash: 26bd037b449a3ca15575
Version: webpack 3.12.0
Time: 1508ms
                                 Asset       Size  Chunks                    Chunk Names
  b19f46f303d02748059251cee2aa6abb.eot    39.6 kB          [emitted]
  a47b89787a4f7491180c40ec69d57998.eot    39.3 kB          [emitted]
  7ed918f3e796962afd05f4a02fb3f84f.eot    40.9 kB          [emitted]
 10d6df6a575fb60ab98d2a7d3792642a.woff      44 kB          [emitted]
  d54d0bf6aa7fcf3279d8812acb322c06.ttf    71.7 kB          [emitted]
  535f5953ba7e46b78cb0242b0e6b8524.svg     118 kB          [emitted]
 43eb7a3417177f8f0fff766082379da5.woff    43.8 kB          [emitted]
  d6175af437d22f14e7cf64c370adc879.ttf    74.5 kB          [emitted]
  7dc8ede1f50d60b9bb7af2b78377f8bf.svg     112 kB          [emitted]
 1c130d09324dbeb3ba2e5a2f9d788a6f.woff    45.4 kB          [emitted]
  5294d5d521aab523c8acd8ac96cfe4c0.ttf    70.9 kB          [emitted]
  1b4b875bc8327f9c37b7288df379b3b5.svg     119 kB          [emitted]
  674f50d287a8c48dc19ba404d20fe713.eot     166 kB          [emitted]
af7ae505a9eed503f8b8e6982036873e.woff2    77.2 kB          [emitted]
 fee66e712a8a08eef5805a46892932ad.woff      98 kB          [emitted]
  b06871f281fee6b241d60582ae9369b9.ttf     166 kB          [emitted]
  912ec66d7572ff821749319396470bde.svg     444 kB          [emitted]  [big]
                              app.html  793 bytes          [emitted]
                                app.js    2.82 kB       0  [emitted]         app
                            styles.css     219 kB       0  [emitted]         app
                            app.js.map    3.09 kB       0  [emitted]         app
                        styles.css.map   87 bytes       0  [emitted]         app
   [0] ./app.js 62 bytes {0} [built]
   [1] ./app.html 54 bytes {0} [built]
   [2] ./site.sass 41 bytes {0} [built]
   [5] ./node_modules/css-loader!./pandoc-solarized.css 3.15 kB [built]
    + 21 hidden modules
Child extract-text-webpack-plugin:
     17 assets
       [5] ./node_modules/css-loader!./node_modules/sass-loader/lib/loader.js!./site.sass 227 kB {0} [built]
       [6] ./node_modules/css-loader!./pandoc-solarized.css 3.15 kB {0} [built]
        + 21 hidden modules
✨  Done in 1.93s.
```

### Watching

```
> stack exec site watch
Listening on http://127.0.0.1:8000
Initialising...
  Creating store...
  Creating provider...
  Running rules...
Checking for out-of-date items
Compiling
  updated css/10d6df6a575fb60ab98d2a7d3792642a.woff
  updated css/1b4b875bc8327f9c37b7288df379b3b5.svg
  updated css/1c130d09324dbeb3ba2e5a2f9d788a6f.woff
  updated css/43eb7a3417177f8f0fff766082379da5.woff
  updated css/5294d5d521aab523c8acd8ac96cfe4c0.ttf
  updated css/535f5953ba7e46b78cb0242b0e6b8524.svg
  updated css/674f50d287a8c48dc19ba404d20fe713.eot
  updated css/7dc8ede1f50d60b9bb7af2b78377f8bf.svg
  updated css/7ed918f3e796962afd05f4a02fb3f84f.eot
  updated css/912ec66d7572ff821749319396470bde.svg
  updated css/a47b89787a4f7491180c40ec69d57998.eot
  updated css/af7ae505a9eed503f8b8e6982036873e.woff2
  updated css/app.html
  updated css/app.js
  updated css/app.js.map
  updated css/b06871f281fee6b241d60582ae9369b9.ttf
  updated css/b19f46f303d02748059251cee2aa6abb.eot
  updated css/d54d0bf6aa7fcf3279d8812acb322c06.ttf
  updated css/d6175af437d22f14e7cf64c370adc879.ttf
  updated css/fee66e712a8a08eef5805a46892932ad.woff
  updated css/styles.css
  updated css/styles.css.map
Success
```

### Cleaning

```
> stack exec site clean
Removing _site...
Removing _cache...
Removing _cache/tmp...
```

### Building

```
> stack exec site build
Initialising...
  Creating store...
  Creating provider...
  Running rules...
Checking for out-of-date items
Compiling
  updated templates/post.html
  updated templates/default.html
  updated templates/nav.html
  updated templates/banner.html
  updated templates/copyright.html
  updated posts/2013-04-07-video-capture-port.md
  updated posts/2018-04-29-block-scope.md
  updated posts/2018-11-28-request binding.md
  updated templates/archive.html
  updated archive.html
  updated css/10d6df6a575fb60ab98d2a7d3792642a.woff
  updated css/1b4b875bc8327f9c37b7288df379b3b5.svg
  updated css/1c130d09324dbeb3ba2e5a2f9d788a6f.woff
  updated css/43eb7a3417177f8f0fff766082379da5.woff
  updated css/5294d5d521aab523c8acd8ac96cfe4c0.ttf
  updated css/535f5953ba7e46b78cb0242b0e6b8524.svg
  updated css/674f50d287a8c48dc19ba404d20fe713.eot
  updated css/7dc8ede1f50d60b9bb7af2b78377f8bf.svg
  updated css/7ed918f3e796962afd05f4a02fb3f84f.eot
  updated css/912ec66d7572ff821749319396470bde.svg
  updated css/a47b89787a4f7491180c40ec69d57998.eot
  updated css/af7ae505a9eed503f8b8e6982036873e.woff2
  updated css/app.html
  updated css/app.js
  updated css/app.js.map
  updated css/b06871f281fee6b241d60582ae9369b9.ttf
  updated css/b19f46f303d02748059251cee2aa6abb.eot
  updated css/d54d0bf6aa7fcf3279d8812acb322c06.ttf
  updated css/d6175af437d22f14e7cf64c370adc879.ttf
  updated css/fee66e712a8a08eef5805a46892932ad.woff
  updated css/styles.css
  updated css/styles.css.map
  updated images/WedgieAttackAvatar.JPG
  updated index.html
  updated templates/blockscope.html
  updated templates/blockscope-contact.html
  updated static/b/index.md
  updated templates/philderbeast.html
  updated templates/philderbeast-contact.html
  updated static/p/index.md
  updated templates/tag.html
  updated templates/post-list.html
  updated tags/compsci.html
  updated tags/conversion.html
  updated tags/fsharp.html
  updated tags/naming.html
  updated tags/parsing.html
  updated tags/servicestack.html
  updated tags/xamarin.html
  updated templates/post-item.html
Success
```

### Deploying

```
> git checkout develop
... (make changes and commit on develop)
> stack exec site clean
> yarn run pack
> stack exec site build
> git checkout master
> cp -a _site/. .
... (commit changes brought over from develop via copy from _site onto master)
> git push
```
