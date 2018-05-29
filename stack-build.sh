#!/bin/bash

mkdir -p result/bin \
  && mkdir -p result/static \
  && echo ">>> Building the frontend..." \
  && stack build --stack-yaml=frontend/stack.yaml \
  && echo ">>> Copying over all.js" \
  && cp $(stack path --stack-yaml=frontend/stack.yaml --local-install-root)/bin/frontend.jsexe/all.js result/static/all.js \
  && echo "" \
  && echo ">>> Building the backend..." \
  && stack build --stack-yaml=backend/stack.yaml \
  && cp $(stack path --stack-yaml=backend/stack.yaml --local-install-root)/bin/backend result/bin/server \
  && echo ">>> Done"
