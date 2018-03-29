#!/bin/bash

# Build the frontend.
stack build --stack-yaml=frontend/stack.yaml
# Copy over the javascript.
cp $(stack path --stack-yaml=frontend/stack.yaml --local-install-root)/bin/frontend.jsexe/all.js result/static/all.js

# Build the backend.
stack build --stack-yaml=backend/stack.yaml
# Copy over the server.
cp $(stack path --stack-yaml=backend/stack.yaml --local-install-root)/bin/backend result/bin/server
