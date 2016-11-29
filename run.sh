#!/bin/bash

rm -rf scripts
mkdir scripts
elm-live src/App.elm --open --output=scripts/asteroids.js
