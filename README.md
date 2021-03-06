# LLCurry

An experimental compiler from [ICurry](https://git.ps.informatik.uni-kiel.de/curry-packages/icurry) (`.icy`) to [LLVM IR](https://llvm.org/docs/LangRef.html) (`.ll`).

The generated LLVM IR code [interfaces](examples/llvm/runtime-interfacing.ll) with the runtime, which is [written in C](runtime/runtime.c).

> Note: The compiler is highly unfinished, mostly intended for experimentation and cannot correctly translate most Curry programs yet.

## Prerequisites

* A recent Curry compiler (e.g. [PAKCS](https://git.ps.informatik.uni-kiel.de/curry/pakcs))
* The Curry Package Manager (included with PAKCS)
* Clang (for compiling the runtime to LLVM IR)

## Usage

<!-- TODO: Document how to build the runtime! -->

To use this compile, first make sure to have a recent Curry compiler (e.g. PAKCS) and the Curry Package Manager (CPM) installed. Then run

```bash
cypm install
```

in this repo to install the dependencies, compile the project and place the `llcurry` executable in a central location. If your `~/.cpm/bin` folder is on your `PATH`, you can now open a directory containing a Curry source file/module and run:

```
icurry YourModule
llcurry YourModule
```

This will first compile the module to ICurry and then to LLVM IR. If everything went well, the resulting `YourModule.ll` file will be located in a subfolder of `.curry`.

## Development

### REPL

For development, it is often useful to test individual functions in the REPL. To do this, just launch

```
pakcs
```

(or another Curry compiler) and load some modules, e.g.

```
:l LLCurry.IR.Example
:add LLCurry.IR.Pretty
:add Text.Pretty
```

Now you can, for example, output an example LLVM IR program:

```curry
putStrLn $ pPrint $ pretty helloWorldLLProg
```

### Fast Recompilation

If no dependencies have changed since your last `install`, you can recompile the program more quickly by running

```
cypm install -x
```

This will skip the installation of package dependencies and directly compile/install `llcurry`.
