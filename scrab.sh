
# kill the running server if it exists
pid=$(lsof -i:8000 -t)
kill -TERM $pid 2> /dev/null || kill -KILL $pid 2> /dev/null


# build and run wai-server in the background
cd ~/Code/Scrabble
stack build
stack exec wai-server&

# build gui, open two tabs for testing
cd ~/Code/elm/scrabble-gui
elm make src/Main.elm --output=app.js
open index.html
open index.html
