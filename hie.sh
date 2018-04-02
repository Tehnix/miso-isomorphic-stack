#!/usr/bin/env bash
DEBUG=1
indent=""
function debug {
  if [[ $DEBUG == 1 ]]; then
    echo "$indent$@" >> /tmp/hie-custom-wrapper.log
  fi
}

curDir=`pwd`
debug "Launching HIE for project located at $curDir"
indent="  "


# Launch the hie for the LTS compiler (assuming built with --copy-compiler-tools).
debug `stack exec -- which hie`
# stack exec -- hie --lsp --debug -l /tmp/hie.log
stack exec -- hie --lsp
