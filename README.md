# clj-media

View or create videos and gifs with clojure.

Uses [avclj](https://github.com/cnuernber/avclj) under the hood.

## Usage

You must have libavcodec installed to use this library.

### Dependency

Currently only available via git dep.
```clojure
com.phronemophobic/clj-media {:git/sha "53a3c2015af973ec67dec036e08086cfac0d2107"
                              :git/url "https://github.com/phronmophobic/clj-media"}
```

### Generating gifs

See [examples/codetogif](examples/codetogif).

### Generate test video

```bash
clojure -X:skia com.phronemophobic.clj-media.skia/gen-test-video
```

Note: currently only skia supported.

### Play Video

Open a window and start playing a video.

#### Skia

```bash
clojure -M:skia -m com.phronemophobic.clj-media.skia my-movie.mp4
```

#### Java Swing

```bash
clojure -M -m com.phronemophobic.clj-media.java2d my-movie.mp4
```

#### Cljfx

```bash
clojure -M:cljfx -m com.phronemophobic.clj-media.cljfx my-movie.mp4
```

#### Skija

```bash
clojure -M:skija -m com.phronemophobic.clj-media.skija my-movie.mp4
```

## Status

Working examples for skia, swing, and cljfx. A reasonable API is still a WIP. Currently, basic support for video and audio, but synced video/audio is still a WIP.

## License

Copyright © 2021 Adrian

Distributed under the Eclipse Public License version 1.0.
