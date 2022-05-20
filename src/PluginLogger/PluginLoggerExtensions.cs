using System;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.DependencyInjection.Extensions;
using Microsoft.Extensions.Logging;

namespace DotNetLightning.ClnRpc.Plugin
{
  public static class PluginLoggerExtensions
  {
    public static ILoggingBuilder AddPluginLogger(this ILoggingBuilder builder, Action<PluginLoggerOptions> configure)
    {
      builder.Services.TryAddEnumerable(ServiceDescriptor.Singleton<ILoggerProvider, PluginLoggerProvider>(_ => new PluginLoggerProvider(configure)));
      return builder;
    }

    public static ILoggingBuilder AddPluginLogger(this ILoggingBuilder builder) =>
      AddPluginLogger(builder, _ => {});
  }
}
