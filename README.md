# spcgame
Asteroids in Elm as a learning exercise

Requires elm 0.18

To get started (**Only tested on a Mac**):
clone the repo
cd to the top level of the repo
type `./run`
This should respond with something like:
~~~~
Success! Compiled 0 modules.                                        
Successfully generated scripts/asteroids.js

elm-live:
  elm-make has succeeded. Starting the server! We’ll open your app
  in the default browser as soon as it’s up and running.

[0000] info  Server running at http://localhost:8000/ (connect)
[0000] info  LiveReload running on 35729
[0001] 12ms          0B GET    304 /
[0001] 3ms           0B GET    304 /style.css
[0001] 19ms        66KB GET    200 /scripts/asteroids.js
~~~~

It should automatically open a browser session (**Only tested with Chrome on a Mac**).  

If it doesn't then open a browser and point it at: [http://localhost:8000](http://localhost:8000)
It will take a while to start because it will initially download the elm-package dependencies and compile the code.
