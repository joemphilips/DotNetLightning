#!/usr/bin/env bash
set -u

dotnet restore --source https://www.myget.org/F/joemphilips/api/v2 --source https://www.myget.org/F/joemphilips/api/v3/index.json --source https://api.nuget.org/v3/index.json
