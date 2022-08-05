using System;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using DotNetLightning.ClnRpc.Plugin;

var builder = Host.CreateDefaultBuilder();
builder
  .ConfigureServices(b => {
    b.AddSingleton<HelloWorldPlugin.HelloWorldPlugin>();
  })
  .ConfigureLogging(loggingBuilder => {
    loggingBuilder.AddPluginLogger();
  });

var app = builder.Build();

if (Environment.GetEnvironmentVariable("LIGHTNINGD_PLUGIN") != "1")
  throw new Exception("helloworld can only be used as a c-lightning plugin.");

var plugin = app.Services.GetService<HelloWorldPlugin.HelloWorldPlugin>();
var _ = await plugin!.StartAsync();

if (plugin.InitializationStatus == PluginInitializationStatus.InitializedSuccessfully)
    await app.RunAsync();
