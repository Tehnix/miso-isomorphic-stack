# Miso Isomorphic Example using Stack
This is an expansion of the [Single page isomorphic example](https://github.com/FPtje/miso-isomorphic-example), which is a minimal example of Miso's isomorphic features.

This example focuses primarily on two things:

- Using the most current version of Miso (at the time of writing, version _0.17.0_)
- Only needing stack, instead of nix (GHCJS is full of nix, so nice with an alternative)
- Make it play nicely with Editor tooling such as HIE.

For more in-depth information, I recommend checking out the [Miso isomorphic example](https://github.com/FPtje/miso-isomorphic-example), which links to some great resources.


__Get Started:__

```bash
$ ./start-build.sh
```

and optionally, if using VSCode and you want to use the [phoityne debugger](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode),

```bash
$ (cd backend && stack build phoityne-vscode) \
  && (cd common && stack build phoityne-vscode)
```

## Running the example
Using stack,

```bash
$ ./stack-build.sh
$ cd result && bin/server
```

__NOTE:__ Same as the original isomorphic example, the it expects you start the server standing in `result/`, so it knows where to find the static files, in `result/static/`.

## Code Structure
Each folder has a `stack.yaml`, which tells stack how to build the code. For convenience, you can build the whole project by using the `stack-build.sh` script, which also copies the files into the result directory.

As mentioned, the executable and JavaScript goes into the `result/` directory, with the JS in `result/static/all.js` and the exec in `result/bin/server`.

#### Frontend
The Miso frontend application, which initialises and runs the app, is located in `frontend/src/Main.hs`.

#### Backend
The Servant backend application, which takes care of routing and serving resources, is located in `backend/src/App.hs`, and tests in `backend/test/Spec.hs`. Finally, the executable is made from `backend/bin/Main.hs`.

#### Common
Finally, the main bulk of the application is found in `common/src/Common/`. In this module we have,

- `Model.hs` contains the `Model`, `initialModel` and update `Action`s.
- `Routes.hs` sets up the links and servant routing trees.
- `View.hs` contains all the views, and a view router (`viewModel`), which takes care of displaying the correct view, or a `page404View`.

#### Project
The _project_ folder is simply a way to get the root files into the workspace, without putting the whole root folder in the workspace. You can `cd project` and then `ln -s ../originalFile lnFile` to get your files in here.

## Development Environment
To open the project, open the `Miso-Project.code-workspace`, which is a VSCode Workspace. This will open the three folders, _backend_, _common_ and _frontend_ as folders in the workspace, and their individual settings will then take effect (this uses VSCodes multi-root workspaces).

__NOTE:__ Currently the focus has been on getting a nice development environment set up for VSCode out-the-box, but there is nothing stopping you from using this with other editors, since it simply uses HIE and stack for everything.

#### Backend and Common
The _backend_, along _common_, can be developed with HIE, by placing a `stack.yaml` in their respective folders. The _backend_ needs this, but for _common_ we have only put one there to make HIE work nicely with it.

Nothing needs to be done to setup these, as long as you opened the project via `Miso-Project.code-workspace` and installed the recommend extensions it lists.


#### Frontend
For the _frontend_ (GHCJS) I haven't find anything really satisfying yet, so I'm settling for running `stack build` on file changes, with the following,

```bash
$ stack --stack-yaml=frontend/stack.yaml build --fast --file-watch
```

You can set this up in an external terminal, or just in VSCode's integrated terminal

### Tasks
Instead of remembering how to [build and run](#running-the-example) and how to [build/watch](#frontend) the frontend, some default tasks are set up.

- `stack-build` runs `./stack-build.sh`, accessible via the `F6` tasks menu
- `stack-build-and-run` runs `./stack-build.sh && cd result && bin/server`, accessible via the `F6` tasks menu and `CMD + SHIFT + b`
- `watch-frontend` runs `stack --stack-yaml=frontend/stack.yaml build --fast --file-watch`, accessible via the `F6` tasks menu

They all run in the correct directory. You can configure these in `backend/.vscode/tasks.json`.

## Miscellaneuous

#### Clean up File Tree Clutter
You can filter out files in the file tree that you rarely, if ever, access. Go into `Settings -> Workspace Settings`, and uncomment the lines you want/add new lines to the `"files.exclude"` object.

#### Using the Debugger
The documentation on [phoityne](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode) is a bit sparse, but the setup here should work. It works by running `backend/test/Spec.hs`, and then follows through the code paths you activate here.

You can for example try and set a breakpoint on line 25 in `common/src/Common/View.hs` and then press `F5` to initialize the debugger and then press continue (or `F5` again). You should now be at the breakpoint you just set.
