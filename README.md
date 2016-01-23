# SCMXREF -- Make cross reference of Gauche program.
## Try without installation:

### 1. Making html files.
```
        $ make demo1
```
will create html directory, which will be something like [this](http://shkmr.github.io/gauche/scmxref/).

### 2. Source browsing
```
        $ make demo2
```
will start http server.
You need to have [Makiki](https://github.com/shirok/Gauche-makiki) installed to run this demo.

## Usage

### `scmxref-html [-d dest-dir] file.scm [file2.scm ...]`

Create html files from file.scm ... into dest-dir.
For example, [this](http://shkmr.github.io/gauche/scmxref/) is the result of
```
   $ scmxref-html scmxref scmxref/*.scm
```

### `scmxref-makiki [-p port ] [file2.scm ...]`

Web server version. Index is dynamically created.


## BUGS

There's only one name space for defined names.
No distinction between module name and global binding.
If a name is defined multiple times, definition
last found was taken. This is not good behavior
especially for define-method.
