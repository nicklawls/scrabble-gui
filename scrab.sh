
# kill the running server if it exists
pid=$(lsof -i:8000 -t)
kill -TERM $pid 2> /dev/null || kill -KILL $pid 2> /dev/null


# build and run wai-server in the background
cd ~/Code/Scrabble
stack build
stack exec wai-server&

# build gui
cd ~/Code/elm/scrabble-gui
elm make src/Main.elm --output=app.js

# kill the existing tabs, because chrome crashes if they're open for some reason
for i in $(chrome-cli list tabs | grep Scrabble | sed 's/.*://' | sed 's/].*//'); do
    chrome-cli close -t $i;
done

# open two new tabs
open index.html
open index.html
