## Code To Gif

A basic example of creating gifs using clj-media.

![repl](repl.gif?raw=true)

![repl-gif-source](repl-gif-code.gif?raw=true)

## Usage

See the docs for `code-to-gif/make-code-gif` and `code-to-gif/repl-to-gif` for programmtic usage.

### Cli example

To run a basic example:

```sh
clojure -M -m code-to-gif
```

This will create `example.gif` with the source of `clojure.core/filter`.

To create a gif for a different function, simply pass the symbol as the first argument:

```sh
clojure -M -m code-to-gif clojure.pprint/pprint
```
