# clj-media

Read, write, and transform audio and video with Clojure.

Powered by [FFmpeg](https://ffmpeg.org/) and [clong](https://github.com/phronmophobic/clong).


## Dependency

```clojure
com.phronemophobic/clj-media {:mvn/version "2.2"}
```

## Documentation

[Guide](https://phronmophobic.github.io/clj-media/)  
[API docs](https://phronmophobic.github.io/clj-media/reference/)

## Locally compiled FFmpeg

clj-media also supports using a locally compiled FFmpeg build.

Example compilation:
```bash
git clone https://github.com/FFmpeg/FFmpeg.git
cd FFmpeg
./configure  --extra-ldflags='-Wl,-ld_classic' --enable-shared --prefix=`pwd`/build
make
make install
```

To use the local build:
1. Add the libraries to the JNA library path
2. Exclude the ffmpeg dependency, `org.bytedeco/ffmpeg-platform`

Example alias:
```edn
{:aliases
 {:local-ffmpeg
  {:jvm-opts ["-Djna.library.path=/path/to/FFmpeg/build/lib"]
   :deps {com.phronemophobic/clj-media {:mvn/version "2.2"
                                        :exclusions [org.bytedeco/ffmpeg-platform]}}}}}
```

## "Roadmap"

Features get added as time and motivation allows. If you would be excited to see a particular feature, please file an issue and let me know!

- [ ] Reading/writing metadata.
- [X] Add support for choosing different ffmpeg builds.
- [X] Improve memory usage.
- [ ] Fix reflection warnings.
- [X] Add support for creating media from raw samples.
- [ ] Add more options for configuring output codecs.
- [ ] Add iterator+autocloseable to frames interface.
- [ ] Support transformation specification by copy and pasting ffmpeg commands.
- [X] Add support for creating media from byte buffers.

## License

Copyright © 2023 Adrian

The contents of this repository may be distributed under the Apache License v2.0 or the GPLv2.
