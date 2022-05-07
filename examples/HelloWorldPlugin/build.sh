#!/usr/bin/env bash

set -eu

RID=""
if [[ $(uname) = Darwin ]]; then
  RID=osx-x64
else
  RID=linux-x64
fi

output_dir="publish"
echo "publishing into ${output_dir} for rid: ${RID}...\n"

dotnet publish -c Debug \
    -o $output_dir \
    -p:PublishReadyToRun=true \
    -p:PublishSingleFile=true \
    -p:RuntimeIdentifier=$RID \
    -p:IncludeNativeLibrariesForSelfExtract=true \
    --self-contained true

