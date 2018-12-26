# Smallpt-scala [![Build Status](https://travis-ci.org/pierceh89/smallpt-scala.svg?branch=master)](https://travis-ci.org/pierceh89/smallpt-scala)

This is scala port of [smallpt](http://www.kevinbeason.com/smallpt/)

## How to run

```bash
# using scalac
scalac src/main/scala/io/github/pierceh89/smallpt/Smallpt.scala
scala io.github.pierceh89.smallpt.Smallpt    #  4 subpixels, default
scala io.github.pierceh89.smallpt.Smallpt 32 # 32 subpixels

# using sbt
sbt
> compile
> run    #  4 subpixels, default
> run 32 # 32 subpixels
```

