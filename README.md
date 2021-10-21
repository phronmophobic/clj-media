# clj-media

Using [avclj](https://github.com/cnuernber/avclj) for working with video.

## Usage

### Generate test video

```bash
clojure -X:skia com.phronemophobic.clj-media.skia/gen-test-video
```


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
clojure:cljfx -M -m com.phronemophobic.clj-media.cljfx my-movie.mp4
```

## Status

Working examples for skia, swing, and cljfx. A reasonable API is still a WIP. Currently, only video works, but future support for audio is intended.

## License

Copyright © 2021 Adrian

Distributed under the Eclipse Public License version 1.0.
