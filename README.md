# Scrabble
---

A gui for [Scrabble](https://github.com/joshcough/Scrabble), implemented in [Elm](elm-lang.org)


### Build and run


Assuming you have `elm` 0.16 installed, change into the directory
and run the following commands.


```
elm package install
elm make src/Main.elm --output app.js
```

You'll also need to install and run the Scrabble server; its probably a good idea to go off of my fork to ensure you have the latest version compatible with the gui.

```
git clone https://github.com/nicklawls/Scrabble
cd Scrabble
stack build
stack exec wai-server
```

Now open `scrabble-gui/index.html` in two separate browser tabs, enter a name in each, and you'll see the game start once both players are signed in. Other than that, not much else is implemented yet!


### Viewing components in the reactor


I also plan to include Main files for viewing and debugging individual components. The easiest way to see these is to run `elm reactor --port 8001`, and click on any `Main.elm` files you find down in the component hierarchy (Note: This doesn't work for the top level Main because it relies on ports and bespoke javascript from the `index.html` file, hence the convoluted build process described above)
