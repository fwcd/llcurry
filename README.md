# LLCurry

An experimental compiler from [ICurry](https://git.ps.informatik.uni-kiel.de/curry-packages/icurry) (`.icy`) to [LLVM IR](https://llvm.org/docs/LangRef.html) (`.ll`).

## Usage

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
