Code accompanying my plugin case study article about Pluggy.

This project reproduces the fictional "htmlize" tool with plugin capabilities
that's repeated throughout the series.

To observe this in action, you have to *install* htmlize and its plugins into
a Python environment. For example, from this directory:

```shell
$ python3 -mvenv <DIR>
$ . <DIR>/bin/activate
$ uv pip install -e htmlize
$ uv pip install -e plugins/tt
$ uv pip install -e plugins/narcissist
$ htmlize_main < sampletext.txt
```

Will process `sampletext.txt` with the plugins activated.
