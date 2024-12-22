# clj-media

Read, write, and transform audio and video with Clojure.

Powered by [FFmpeg](https://ffmpeg.org/) and [clong](https://github.com/phronmophobic/clong).


## Dependency

```clojure
com.phronemophobic/clj-media {:mvn/version "2.3"}
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
   :deps {com.phronemophobic/clj-media {:mvn/version "2.3"
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

## Status

The domain of audio/video has quite a bit of complexity. FFmpeg adds to the complexity in its own way, as does clj-media. I think there's still a lot of room for improvement for clj-media. I regularly revisit clj-media and make updates as I slowly grok more of the domain and FFmpeg. Part of the problem is that while the FFmpeg source has some documentation, it's almost all oriented towards the command line interface. I haven't figured out a better way to learn how to use the FFmpeg native API other than reading the source and [examples](https://github.com/FFmpeg/FFmpeg/tree/master/doc/examples). 

There are many areas of improvement for clj-media.
- The initial implementation of filters intended to lean on the garbage collector for memory management. This often works well for wrapping native libraries, but it does not work well for clj-media. Unfortunately, I found this out after writing and testing the initial implementation. I did go back and rewrite portions for use cases where relying on GC was especially egregious, but didn't have time to rewrite everything. In practice, that means there are two separate implementations for similar uses. This doesn't affect the high level API, but makes the low level API much more confusing. At some point, the last vestiges of the GC based API should be reimplemented and/or removed.
- The high level API currently is based on transforming media (collections of streams) rather than transforming individual streams. This is similar to python media libraries I've used worked, but I'm not sure working with media rather than streams is much easier except in the simplest cases.
- The use case of playing audio/video is well supported. I'm imagining some sort of API that has `play`, `pause`, `stop` that can feed some process frames.
- clj-media can only read compressed media from the filesystem. The FFmpeg API allows for other input sources which would be useful for loading media from memory or the network.

## License

Copyright © 2023 Adrian

The contents of this repository may be distributed under the Apache License v2.0 or the GPLv2.
