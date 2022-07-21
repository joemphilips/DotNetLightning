using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DotNetLightning.ClnRpc.Plugin;
using Microsoft.Extensions.Logging;
using Microsoft.FSharp.Core;
using NBitcoin;

namespace HelloWorldPlugin
{
  /// <summary>
  /// An example plugin, the behavior must be the same with
  /// https://github.com/ElementsProject/lightning/blob/master/contrib/plugins/helloworld.py
  /// except that it will notify to c-lightning about its own custom notification topic "parted_from_world"
  /// When a user calls "bye" rpc.
  /// </summary>
  public class HelloWorldPlugin : PluginServerBase
  {
    private readonly ILogger<HelloWorldPlugin> _logger;
    private readonly IHostApplicationLifetime _applicationLifetime;

    private string greeting = "Hello";

    public HelloWorldPlugin(ILogger<HelloWorldPlugin> logger, IHostApplicationLifetime applicationLifetime):
      base(new []{ "parted_from_world" }, false)
    {
      _logger = logger ?? throw new ArgumentNullException(nameof(logger));
      _applicationLifetime = applicationLifetime;
      this.Network = Network.RegTest;
      this.Options = new [] {
        new PluginOptions {
          Name = "greeting",
          Default = greeting,
          Description = "The greeting I should use",
          OptionType = PluginOptType.String,
          Multi = false,
          Deprecated = false
        }
      };
    }


    public override void InitCore(
      LightningInitConfigurationDTO configuration,
      Dictionary<string, object> cliOptions)
    {
      greeting = "Hello";
      if (cliOptions.TryGetValue("greeting", out var greetingValue))
        greeting = (string)greetingValue;
    }

    public override FeatureSetDTO FeatureBits { get; set; }

    sealed public override IEnumerable<PluginOptions> Options { get; set; }

    const string HelloLongDesc =
      "This is a documentation for \"hello\" rpc method. It gets reported when registering the" +
      "function as a method with `lightningd`";

    [PluginJsonRpcMethod("hello", "This is a documentation string for the hello-function", HelloLongDesc)]
    public async Task<string> HelloAsync(string name = "world")
    {
      using (await this.AsyncSemaphore.EnterAsync())
      {
        var s = $"{greeting}, {name}";
        _logger.LogInformation(s);
        return s;
      }
    }

    [PluginJsonRpcSubscription("shutdown")]
    public void Shutdown()
    {
        _applicationLifetime.StopApplication();
    }
    

    [PluginJsonRpcSubscription("connect")]
    public async Task OnConnectAsync(string id, string address)
    {
      using (await this.AsyncSemaphore.EnterAsync())
        _logger.LogInformation($"Received connect event for peer {id}");
    }

    [PluginJsonRpcSubscription("disconnect")]
    public async Task OnDisconnectAsync(string id)
    {
      using (await this.AsyncSemaphore.EnterAsync())
        _logger.LogInformation($"Received disconnect event for peer {id}");
    }

    [PluginJsonRpcSubscription("invoice_payment")]
    public async Task OnPaymentAsync(string label, string preimage, long msat)
    {
      using (await this.AsyncSemaphore.EnterAsync())
        _logger.LogInformation($"Received invoice_payment event for label {label}, preimage {preimage}, and" +
                               $"amount amount of {msat}");
    }
  }
}
