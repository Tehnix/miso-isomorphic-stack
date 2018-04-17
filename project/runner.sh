#!/bin/sh
RESULT="$(pwd)/result"

sigIntHandler() {
  echo ""
  echo "[Runner] Killing $@..."
  kill $PID
  exit
}

trap sigIntHandler SIGINT

machine=$(uname)

cd $RESULT

while true; do
  echo "[Runner] Starting $@..."
  $@ &
  PID=$!
  # Wait for the server executable to change, then kill it, and let it
  # start again in the next iteration.
  fswatch -0 -1 "$RESULT/bin/server" | xargs -0 -n 1 -I {} kill $PID
  echo "[Runner] Relaunching $@..."
  echo ""
done
