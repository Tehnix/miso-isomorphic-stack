# Miso Isomorphic Example using Stack
This is an expansion of the [Single page isomorphic example](https://github.com/FPtje/miso-isomorphic-example), which is a minimal example of Miso's isomorphic features.

This example has been updated to use the most current version of Miso at the time of writing, version _0.17.0_, and more importantly only uses stack, instead of relying on nix. The ultimate goal of this, is to provide a stack compatible full setup, that also works nicely with existing tooling. The last bit (tooling) is now the remaining focus of this example.

For more in-depth information, I recommend checking out the [Miso isomorphic example](https://github.com/FPtje/miso-isomorphic-example), which links to some great resources.

## Running the example

Using stack,

```bash
$ .stack-build.sh
$ cd result && bin/server
```

__NOTE:__ Same as the original isomorphic example, the it expects you start the server standing in `result/`, so it knows where to find the static files, in `result/static/`.
