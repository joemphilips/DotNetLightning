#!/usr/bin/env bash
set -u

readonly VERSION="1.0"
if [[ "$(uname)" == 'Darwin' ]]; then
  readonly SCRIPT_DIR_PATH=$(dirname $(greadlink -f $0))
else
  readonly SCRIPT_DIR_PATH=$(dirname $(readlink -f $0))
fi

dotnet restore --source https://www.myget.org/F/joemphilips/api/v2 --source https://www.myget.org/F/joemphilips/api/v3/index.json --source https://api.nuget.org/v3/index.json
