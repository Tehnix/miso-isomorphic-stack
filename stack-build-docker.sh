#!/bin/bash

DOCKER_IMAGE="ghcjs:lts-9.21"

mkdir -p result/bin
mkdir -p result/static

# Build the docker image, if it doesn't exist.
if [[ "$(docker images -q $DOCKER_IMAGE 2> /dev/null)" == "" ]]; then
  echo ">>> Didn't find a docker image name $DOCKER_IMAGE, building one..."
  docker build -t $DOCKER_IMAGE .
fi

echo ">>> Building the frontend..."
# Build the frontend, commit the new image, and copy over the javascript.
docker run -v $(pwd):/src -it $DOCKER_IMAGE stack build --stack-yaml=frontend/stack.yaml \
  && docker commit $(docker ps -l -q) $DOCKER_IMAGE \
  && echo ">>> Copying over all.js" \
  && docker run -v $(pwd):/src -it $DOCKER_IMAGE cp frontend/.stack-work/install/x86_64-linux/lts-9.21/ghcjs-0.2.1.9009021_ghc-8.0.2/bin/frontend.jsexe/all.js result/static/all.js

echo ">>> Building the backend..."
# Build the backend, and copy over the server.
stack build --stack-yaml=backend/stack.yaml \
  && cp $(stack path --stack-yaml=backend/stack.yaml --local-install-root)/bin/backend result/bin/server

echo ">>> Done"
