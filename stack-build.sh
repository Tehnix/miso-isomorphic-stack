#!/usr/bin/env bash

nodePath=$(which node)
if [[ $nodePath == *"nodenv/shims"* ]]; then
  nodePath=$(nodenv which node)
fi

mkdir -p result/bin \
  && mkdir -p result/static \
  && echo "λ Building the frontend..." \
  && stack build --stack-yaml=frontend/stack.yaml \
  && echo "λ Copying over all.js" \
  && cp $(stack path --stack-yaml=frontend/stack.yaml --local-install-root)/bin/frontend.jsexe/all.js result/static/all.js \
  && echo "" \
  && echo "λ Building the backend..." \
  && stack build --stack-yaml=backend/stack.yaml \
  && cp $(stack path --stack-yaml=backend/stack.yaml --local-install-root)/bin/backend result/bin/server \
  && echo "λ Done"
