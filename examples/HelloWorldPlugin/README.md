# Example c-lightning plugin in C#.

## How to test.

Assume you are running your c-lightning in `linux-x64`,

```sh

./build.sh

# note: in real environment, you may want to build with `PublishTrimmed`
# and `Release` configuration.
```

This will create a single file binary under `publish/` named `helloworld`
Move it to your c-lightning plugin directory, and run c-lightning with plugin enabled.
That's it!

