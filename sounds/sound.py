#!/usr/bin/env python3
import math
import wave

framerate = 44100

def write(name, data):
    f = wave.open(name, 'wb')
    f.setnchannels(1)
    f.setsampwidth(2)
    f.setframerate(framerate)
    f.writeframes(data)
    f.close()

def wav(name, duration):
    nsamples = int(duration * framerate)
    def ret(f):
        samples = [int((f(t / framerate) + 1) * 16) for t in range(nsamples)]
        s = bytes(b for s in samples for b in [s >> 8, s & 0xff])
        s += bytes(b for s in range(22050) for b in [0, 0])
        write(name, s)
    return ret

def s(t):
    return math.sin(2 * math.pi * t)

@wav('end.wav', 0.3)
def f(t):
    if t < 0.1:
        return s(t * 880)
    elif t < 0.2:
        return s(t * math.sqrt(440*880))
    else:
        return s(t * 440)

@wav('tick.wav', 0.1)
def f(t):
    return s(t * 880)

@wav('Spades.wav', 0.1)
def f(t):
    return s(t * 240)

@wav('Hearts.wav', 0.1)
def f(t):
    return s(t * 270)

@wav('Diamonds.wav', 0.1)
def f(t):
    return s(t * 320)

@wav('Clubs.wav', 0.1)
def f(t):
    return s(t * 360)

@wav('silence.wav', 0.5)
def f(t):
    return -1
