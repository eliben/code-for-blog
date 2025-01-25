These samples can be run with `uv`:

    uv run add-two-numbers.py

And so on. According to [llvmlite's docs](http://llvmlite.pydata.org/en/latest/admin-guide/install.html)
the pip package `llvmlite` comes with binary wheels, so this will pull the
right LLVM binaries automatically, and there shouldn't be a need to download
and set up LLVM separately from llvmlite.
