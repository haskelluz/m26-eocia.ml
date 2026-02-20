# eocia.ml

Workshop materials for https://youtu.be/Ni4B8KpRca4

OCaml implementation of [*Essentials of Compilation: An Incremental Approach*](https://github.com/IUCompilerCourse/Essentials-of-Compilation) by Jeremy G. Siek.

## Branches

- `master` - starter code
- [`lvar`](https://github.com/haskelluz/m26-eocia.ml/tree/lvar) - solutions for chapters 1-2 (interpreter + LVar compiler)

## Slides

[View the workshop slides](https://haskelluz.github.io/m26-eocia.ml/WORKSHOP.html)

## Building

Requires OCaml and Dune. With Nix:

```
nix develop
```

Then:

```
dune build
dune test
```

## License

Code is licensed under [BSD-3-Clause](LICENSE-CODE).
Workshop content (slides, images) is licensed under [CC-BY-4.0](LICENSE-CONTENT).

*Essentials of Compilation* by Jeremy G. Siek is licensed under [CC-BY-NC-ND](https://creativecommons.org/licenses/by-nc-nd/4.0/).
