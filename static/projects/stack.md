---
title: 'Stack Tooling Contributions'
---
A list of contributions I've made to `commercialhaskell`[**/stack**](https://docs.haskellstack.org):

- **Functionality**; feature additions, bug fixes, continuous integration
    - Help text allows multiple use for `--package`,
      [#5775](https://github.com/commercialhaskell/stack/pull/5775)
    - Use original `YAML` to preserve top-level order,
      [#5813](https://github.com/commercialhaskell/stack/pull/5813)
    - Add stan gh-action and ignored triggered checks,
      [#5811](https://github.com/commercialhaskell/stack/pull/5811)
    - Adds a `cabal.project` file,
      [#5860](https://github.com/commercialhaskell/stack/pull/5860)
    - Check if target might be a project-level configuration file, not a
      missing directory,
      [#6033](https://github.com/commercialhaskell/stack/pull/6033)
    - Add ide target component types,
      [#6126](https://github.com/commercialhaskell/stack/pull/6126)
    - Build _ with `ghc-x.y.z`,
      [#6344](https://github.com/commercialhaskell/stack/pull/6344)
- **Documentation**
    - Document how to suppress the ``-nopie`` warnings on macOS,
      [#4392](https://github.com/commercialhaskell/stack/pull/4392/files)
    - Link to unit tests and integration tests,
      [#5809](https://github.com/commercialhaskell/stack/pull/5809)
- **Trivial**
    - Ignore `dist-newstyle` folder,
      [#5776](https://github.com/commercialhaskell/stack/pull/5776)
    - Lint `YAML` especially `.hlint.yaml`,
      [#5797](https://github.com/commercialhaskell/stack/pull/5797)
    - Typo close parenthesis for work directory default,
      [#5812](https://github.com/commercialhaskell/stack/pull/5812)
    - Bump the cabal project to match `lts-20.0` in `stack.yaml`,
      [#6031](https://github.com/commercialhaskell/stack/pull/6031)
