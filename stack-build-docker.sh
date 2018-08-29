#!/usr/bin/env bash
DOCKER_IMAGE="ghcjs:lts-9.21"

# Build the docker image, if it doesn't exist.
if [[ "$(docker images -q $DOCKER_IMAGE 2> /dev/null)" == "" ]]; then
  echo "λ Didn't find a docker image name $DOCKER_IMAGE, building one..."
  docker build -t $DOCKER_IMAGE .
  echo "λ Copying over the stack root folder to .stack-docker in the project root"
  docker run -v $(pwd):/src -it $DOCKER_IMAGE cp -R /root/.stack /src/.stack-docker
fi


mkdir -p result/bin \
  && mkdir -p result/static \
  && echo "λ Building the frontend..." \
  && docker run -v $(pwd):/src -it $DOCKER_IMAGE stack --stack-root /src/.stack-docker --stack-yaml=frontend/stack.yaml build \
  && echo "λ Copying over all.js" \
  && cp frontend/.stack-work/install/x86_64-linux/lts-9.21/ghcjs-0.2.1.9009021_ghc-8.0.2/bin/frontend.jsexe/all.js result/static/all.js \
  && echo "λ Building the backend..." \
  && stack build --stack-yaml=backend/stack.yaml \
  && cp $(stack path --stack-yaml=backend/stack.yaml --local-install-root)/bin/backend result/bin/server \
  && echo "λ Done"
