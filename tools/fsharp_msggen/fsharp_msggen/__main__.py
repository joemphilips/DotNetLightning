import subprocess
from pathlib import Path

from msggen.gen.generator import GeneratorChain
from msggen.utils import load_jsonrpc_service
from .fsharp import FSharpGenerator, FSharpClientExtensionGenerator
from .fsharp_tests import FSharpPropertyTestsGenerator


def repo_root():
  path = subprocess.check_output(["git", "rev-parse", "--show-toplevel"])
  return Path(path.strip().decode('UTF-8'))


def add_fsharp_generator(dest_project, generator_chain: GeneratorChain):
  dest = open(dest_project / "Requests.fs", "w")
  generator_chain.add_generator(FSharpGenerator(dest))


def add_fsharp_method_generator(dest_project, generator: GeneratorChain):
  dest = open(dest_project / "Client.Methods.fs", "w")
  generator.add_generator(FSharpClientExtensionGenerator(dest))

def add_fsharp_property_test_generator(dest_test_project, generator: GeneratorChain):
  dest = open(dest_test_project / "SerializationTests.fs", "w")
  generator.add_generator(FSharpPropertyTestsGenerator(dest))

def run():
  generator_chain = GeneratorChain()
  dest_project = repo_root() / "src" / "DotNetLightning.ClnRpc"
  add_fsharp_generator(dest_project, generator_chain)
  add_fsharp_method_generator(dest_project, generator_chain)

  dest_test_project = repo_root() / "tests" / "DotNetLightning.ClnRpc.Tests"
  add_fsharp_property_test_generator(dest_test_project, generator_chain)

  schema_dir = repo_root() / "lightning" / "doc" / "schemas"
  service = load_jsonrpc_service(schema_dir)
  generator_chain.generate(service)


if __name__ == "__main__":
  run()
