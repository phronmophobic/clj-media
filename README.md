# clj-media

View or create videos and gifs with clojure.

Powered by [ffmpeg](https://ffmpeg.org/) and [clong](https://github.com/phronmophobic/clong).


### Dependency

Currently only available via git dep.
```clojure
com.phronemophobic/clj-media {:git/sha "53a3c2015af973ec67dec036e08086cfac0d2107"
                              :git/url "https://github.com/phronmophobic/clj-media"}
```

## "Roadmap"

- [ ] Reading/writing metadata.
- [ ] Add support for choosing different ffmpeg builds.
- [ ] Improve memory usage.
- [ ] Fix reflection warnings.
- [ ] Add support for creating media from raw samples.
- [ ] Add more options for configuring output codecs.

## License

Copyright © 2023 Adrian

The contents of this repository may be distributed under the Apache License v2.0 or the GPLv2.
