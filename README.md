# Block Scope

### Styling

```
> pnpm run style

> blockscope@ style /Users/pdejoux/dev/hakyll/blockscope
> sass app.sass:css/app.css --load-path ./node_modules
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
...
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
...
Success
```

### Deploying

```
> git checkout develop
... (make changes and commit on develop)
> stack exec site clean
> pnpm run style
> stack exec site build
> git checkout master
> cp -a _site/. .
... (commit changes brought over from develop via copy from _site onto master)
> git push
```
