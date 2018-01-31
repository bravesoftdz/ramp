```
     ___     _             _         _             ___          _        _
    | _ \___| |_ _ _ ___  /_\  _ __ | |__  ___ _ _| _ \_ _ ___ (_)___ __| |_
    |   / -_)  _| '_/ _ \/ _ \| '  \| '_ \/ -_) '_|  _/ '_/ _ \| / -_) _|  _|
    |_|_\___|\__|_| \___/_/ \_\_|_|_|_.__/\___|_| |_| |_| \___// \___\__|\__|
                                                             |__/
```
---

## About Retro Amber Project
### What is it?
Software that will run on Raspberry Pi and provide following features:
* Shoutcast playback (internet radio)
* Weather forecast
* Clock
* Displaying info on alphanumeric LCD attached to GPIO
* Embedded HTTP server that will host webclient and REST API

For now it's mostly an idea, but it keeps on growing! I've already made prototypes for each of that features and it's time to bundle it up together.

### Why `Retro Amber`?
Mostly because I just *LOVE* old monochromatic displays, especially amber ones like [this](http://www.vintage-computer.com/images/ibmpcportscreen.jpg), also shortcut is `RAmp` and that got `Amp` in it and this often is associated with something audio related (remember `Winamp`?). Finally I just didn't want to start with `Yet Another` in name (because I know there are a quite few projects like this).

### Geez man, why Pascal?
I was struggling with this a bit, and was considering many alternatives, but:
* it's only native alternative to C++
* since it's native - it's fast
* since it's fast - it won't overheat poor Raspberry CPU too much (around ~7% CPU usage while playing audio, grabbing and transforming FFT data, and displaying it 60 times per second on 4x20 HD44870 display, this is trully amazing performance in my opinion)
* crossplatform without a sweat
* quick compilation
* allows me to:
  * access GPIO ports fast
  * use [BASS](http://www.un4seen.com/) library for audio (this lib is amazing!)
  * use code I've wrote ~8 years ago to drive HD44870 display over LPT and port it to GPIO in 5 minutes
* I was hooked up on programming thanks to Pascal and this is a trip down memory lane :heart:

### Future of Retro Amber Project

* Possibly moving to [Ultibo](https://ultibo.org/) (to reduce boot time to almost instant)
* Saving played shoutcast streams to files
* Playing from library of audio files
* I would really like to be able to playback audio from YouTube clips

## How to get it running?
For now just use [Lazarus](https://www.lazarus-ide.org/), open project, and build.
