# Scrabble
---

A gui for [Haskell Scrabble](https://github.com/joshcough/Scrabble), implemented in [Elm](elm-lang.org)


### Build and run


Assuming you have `elm` 0.16 installed, change into the directory
and run the following commands.


```
elm package install
elm make src/Main.elm --output app.js
```

You'll also need to install [Scrabble](https://github.com/joshcough/Scrabble) and run the `wai-server` executable

```
git clone https://github.com/joshcough/Scrabble
cd Scrabble
stack build
stack exec wai-server
```

Now open `scrabble-gui/index.html` in two separate browser tabs, and you'll see them complete the initial handshake with the server.
