Miso Isomorphic Example using Stack
===

This is an expansion of the [SPA isomorphic example](https://github.com/FPtje/miso-isomorphic-example), which is a minimal example of Miso's isomorphic features.

__Get Started__

```bash
$ ./start-build.sh
$ cd result && bin/server
```

That's all you need to do. The rest of this README is for a deeper dive into the development setup, code structure and more.

<!-- TOC -->

- [Motivation](#motivation)
- [Running the example](#running-the-example)
- [Code Structure](#code-structure)
    - [Frontend](#frontend)
    - [Backend](#backend)
    - [Common](#common)
    - [Project](#project)
- [Development Environment](#development-environment)
    - [Without VSCode](#without-vscode)
    - [Backend and Common](#backend-and-common)
    - [Frontend](#frontend-1)
  - [Tasks](#tasks)
- [Miscellaneuous](#miscellaneuous)
    - [Setting up HIE](#setting-up-hie)
    - [Using the Debugger](#using-the-debugger)
    - [Clean up File Tree Clutter](#clean-up-file-tree-clutter)
    - [Default Enabled Extensions](#default-enabled-extensions)
- [TODO](#todo)

<!-- /TOC -->

## Motivation
This example focuses on:

- Only needing stack, instead of nix (GHCJS is full of nix, so nice with an alternative).
- Make it play nicely with Editor tooling such as HIE.
- Use [hpack](https://github.com/sol/hpack) to generate `.cabal` files, keeping common settings in `package-lib.yaml`.
- Using the most current version of Miso (at the time of writing, version _0.18.0_).

Additionally it sets up a nice development environment for [VSCode](https://code.visualstudio.com):
- A list of recommended extensions accessible via `⌘ ⇧ p` and then typing `Extension: Show Recommended Extensions`
- Linting, autocomplete, code hints, formatting, etc via HIE (check out [how to set it up](#setting-up-hie))
- Step-through debugger via [phoityne](#using-the-debugger)
- Automatically run hpack when saving `package.yaml` files via [save-and-run](https://marketplace.visualstudio.com/items?itemName=wk-j.save-and-run)
- Several premade tasks for common things like:
  - Building the whole project, and optionally launch the server
  - Building individual packages
  - Running tests, either once or on file changes

For more in-depth information, I recommend checking out the [Miso isomorphic example](https://github.com/FPtje/miso-isomorphic-example), which links to some great resources.

## Running the example
Using stack,

```bash
$ ./stack-build.sh
$ cd result && bin/server
```

__NOTE:__ Same as the original isomorphic example, the it expects you start the server standing in `result/`, so it knows where to find the static files, in `result/static/`.

If using VSCode and you want to use the [phoityne debugger](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode),

```bash
$ (cd backend && stack build phoityne-vscode) \
  && (cd common && stack build phoityne-vscode)
```

Additionally, you might want `fswatch`, for the `runner.sh`/`rebuild.sh` scripts,

```bash
$ brew install fswatch # alternatively, sudo apt-get install fswatch
```

## Code Structure
Each folder has a `stack.yaml`, which tells stack how to build the code. For convenience, you can build the whole project by using the `stack-build.sh` script, which also copies the files into the result directory.

As mentioned, the executable and JavaScript goes into the `result/` directory, with the JS in `result/static/all.js` and the exec in `result/bin/server`.

#### Frontend
The JavaScript gets generated from the GHCJS frontend:

| Filename | Responsibility | Build Type |
|----------|----------------|------|
| `src/Main.hs` | The Miso frontend application, which initialises and runs the app | Executable |

#### Backend
The Servant backend application, which takes care of routing and serving resources, is located in the _backend_:

| Filename | Responsibility | Build Type |
|----------|----------------|------|
| `bin/Main.hs` | Sets up the WAI/Warp server that runs the servant backend  | Executable |
| `src/App.hs` | The Servant backend application, which takes care of routing and serving resources | Library |
| `test/Main.hs` | Configures the hspec test environemnt | Test |
| `test/Spec.hs` | Automatically finds all test files that end on `Spec.hs` | Test |
| `test/Backend/AppSpec.hs` | Tests for `src/App.hs` (the default setup just checks that the endpoints return status code `200`) | Test |

#### Common
Finally, the main bulk of the application is found in `common/src/Common/`. In this module we have:

| Filename | Responsibility | Build Type |
|----------|----------------|------|
| `src/Common/Model.hs` | Contains the `Model`, `initialModel` and update `Action`s | Library |
| `src/Common/Routes.hs` | Sets up the links and servant routing trees | Library |
| `src/Common/View.hs` | Contains all the views, and a view router (`viewModel`), which takes care of displaying the correct view, or a `page404View` | Library |

#### Project
The _project_ folder is simply a way to get the root files into the workspace, without putting the whole root folder in the workspace. You can `cd project` and then `ln -s ../originalFile lnFile` to get your files in here.

## Development Environment
To open the project, open the `Miso-Project.code-workspace`, which is a VSCode Workspace. This will open the three folders, _backend_, _common_ and _frontend_ as folders in the workspace, and their individual settings will then take effect (this uses VSCodes multi-root workspaces).

__NOTE:__ Currently the focus has been on getting a nice development environment set up for VSCode out-the-box, but there is nothing stopping you from using this with other editors, since it simply uses HIE and stack for everything.

#### Without VSCode
There are four main pieces of this that I would recommend running in your terminal (each in their own),

```bash
$ stack --stack-yaml=backend/stack.yaml build --fast --file-watch
$ stack --stack-yaml=frontend/stack.yaml build --fast --file-watch
$ ./project/rebuild.sh & ./project/runner.sh bin/server
```

That should let you edit your files, and automatically build the backend/frontend, copy the files over and relaunch the server.

#### Backend and Common
The _backend_, along _common_, can be developed with HIE, by placing a `stack.yaml` in their respective folders. The _backend_ needs this, but for _common_ we have only put one there to make HIE work nicely with it.

Nothing needs to be done to setup these, as long as you opened the project via `Miso-Project.code-workspace` and installed the recommend extensions it lists.


#### Frontend
For the _frontend_ (GHCJS) I haven't find anything really satisfying yet, so I'm settling for running `stack build` on file changes, with the following,

```bash
$ stack --stack-yaml=frontend/stack.yaml build --fast --file-watch
```

Or run the VSCode task `Watch Test Frontend` (press `F6`). You can set this up in an external terminal, or just in VSCode's integrated terminal.

### Tasks
Instead of remembering how to [build and run](#running-the-example) and how to [build/watch](#frontend) the frontend, some default tasks are set up.

You often just want to run `Rebuild/Copy/Launch Everything!`, which will start three separate tasks, so you don't have to do anything.

| Name | Command | Description | Keybinding |
|------|---------|-------------|------------|
| `Build Project` | `./stack-build.sh` | Builds the whole project | `⌘ ⇧ b`, `F7` or `F6` task menu |
| `Build Project and Launch Server` | `./stack-build.sh && cd result && bin/server` | Builds the whole project and launches the server | `F6` task menu |
| `Relaunch Server on Change` | `./project/runner.sh bin/server` | Relaunches the server, every time the result/bin/server executable changes | `F6` task menu |
| `Copy Build Files on Change` | `./project/rebuild.sh` | Copies the build files to `result/..` every time the stack build artifacts change | `F6` task menu |
| `Relaunch Server & Copy Build Files on Change` | `./project/rebuild.sh & ./project/runner.sh bin/server` | A combination of the relauncher and rebuilder (copier) | `F6` task menu |
| `Build Backend` | `stack --stack-yaml=backend/stack.yaml build --fast` | Builds the backend | `F6` task menu |
| `Build Frontend` | `stack --stack-yaml=frontend/stack.yaml build --fast` | Builds the frontend | `F6` task menu |
| `Watch Test Backend` | `stack --stack-yaml=backend/stack.yaml test --fast --haddock-deps --file-watch` | Runs tests for the backend on file changes | `F6` task menu |
| `Watch Test Frontend` | `stack --stack-yaml=frontend/stack.yaml test frontend --fast --haddock-deps --file-watch` | Runs tests for the frontend on file changes | `F6` task menu |
| `Watch Build Frontend with Problem Matcher` | `stack --stack-yaml=frontend/stack.yaml build frontend --fast --file-watch` | Builds the frontend on file changes, and reports the errors at the file location they were found | `F6` task menu |
| `Watch Test Common` | `stack --stack-yaml=common/stack.yaml test --fast --haddock-deps --file-watch` | Runs tests for common on file changes | `F6` task menu |
| `Test Backend` | `stack --stack-yaml=backend/stack.yaml test --fast` | Runs tests for the backend | `F8` or `F6` task menu |
| `Test Frontend` | `stack --stack-yaml=frontend/stack.yaml test frontend --fast` | Runs tests for the frontend | `F6` task menu |
| `Test Common` | `stack --stack-yaml=common/stack.yaml test --fast` | Runs tests for the frontend | `F6` task menu |

They all run in the correct directory. You can configure these in `backend/.vscode/tasks.json`, `common/.vscode/tasks.json` and `frontend/.vscode/tasks.json`.

## Miscellaneuous

#### Setting up HIE
The recommended way to setup HIE is to clone it down and then run `make build-copy-compiler-tool`,

```bash
$ git clone https://github.com/haskell/haskell-ide-engine.git \
  && cd haskell-ide-engine \
  && make build-copy-compiler-tool
```

This will, at the time of writing, install HIE for GHC 8.0.2, 8.2.1 and 8.2.2.

You can then find the locations with `stack exec -- which hie`, which will make sure to pick the right `hie` executable for your project (GHC versions need to match).

#### Using the Debugger
The documentation on [phoityne](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode) is a bit sparse, but the setup here should work. It works by running `backend/test/Spec.hs`, and then follows through the code paths you activate here.

You can for example try and set a breakpoint on inside a view in `common/src/Common/View.hs` (e.g. inside `homeView`) and then press `F5` to initialize the debugger and then press continue (or `F5` again). You should now be at the breakpoint you just set.

#### Clean up File Tree Clutter
You can filter out files in the file tree that you rarely, if ever, access. Go into `Settings -> Workspace Settings`, and uncomment the lines you want/add new lines to the `"files.exclude"` object.

#### Default Enabled Extensions
By default a slew of extensions are enabled. This is mainly inspired by Alexis King's writeup [An opinionated guide to Haskell in 2018](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/), which makes good arguments for enabling these.

## TODO
There's still some things that would be nice to have included here.

- [ ] Database setup and some simple CRUD operations in the backend (e.g. using presistent[1](https://github.com/parsonsmatt/servant-persistent)[2](http://www.parsonsmatt.org/2016/07/08/servant-persistent_updated.html))
- [ ] [XHR requests](https://github.com/dmjio/miso/blob/master/examples/xhr/Main.hs) in the frontend, do show how to communicate with the backend
