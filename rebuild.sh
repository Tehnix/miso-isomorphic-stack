#!/bin/sh

sigIntHandler() {
  echo ""
  echo "[Runner] Killing rebuilder..."
  exit
}

trap sigIntHandler SIGINT

machine=$(uname)

JSDIR=$(stack path --stack-yaml=frontend/stack.yaml --local-install-root)/bin/frontend.jsexe
SERVERDIR=$(stack path --stack-yaml=backend/stack.yaml --local-install-root)/bin


# Copy the build files into place whenver the stack build artifacts change.
fswatch -0 "$JSDIR/all.js" "$SERVERDIR/backend" | xargs -0 -n 1 -I {} \
  echo "Copying build files..." \
  && (cp "$SERVERDIR/backend" result/bin/server & cp "$JSDIR/all.js" result/static/all.js)
