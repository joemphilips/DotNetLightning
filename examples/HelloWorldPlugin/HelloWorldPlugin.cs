using DotNetLightning.ClnRpc.Plugin;
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

    private string _greeting = "Hello";

    public HelloWorldPlugin(ILogger<HelloWorldPlugin> logger, IHostApplicationLifetime applicationLifetime):
      base(new []{ "parted_from_world" }, false)
    {
      _logger = logger ?? throw new ArgumentNullException(nameof(logger));
      _applicationLifetime = applicationLifetime;
      this.Network = Network.RegTest;
      this.Options = new [] {
        new PluginOptions {
          Name = "greeting",
          Default = _greeting,
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
      _greeting = "Hello";
      if (cliOptions.TryGetValue("greeting", out var greetingValue))
        _greeting = (string)greetingValue;
    }

    public override FeatureSetDTO? FeatureBits { get; set; } = null;

    sealed public override IEnumerable<PluginOptions> Options { get; set; }

    const string HelloLongDesc =
      "This is a documentation for \"hello\" rpc method. It gets reported when registering the" +
      "function as a method with `lightningd`";

    [PluginJsonRpcMethod("hello", "This is a documentation string for the hello-function", HelloLongDesc)]
    public Task<string> HelloAsync(string name = "world")
    {
      var s = $"{_greeting}, {name}";
      _logger.LogInformation(s);
      return Task.FromResult(s);
    }

    [PluginJsonRpcSubscription("shutdown")]
    public void Shutdown()
    {
        _applicationLifetime.StopApplication();
    }
    

    [PluginJsonRpcSubscription("connect")]
    public Task OnConnectAsync(string id, string address)
    {
      _logger.LogInformation($"Received connect event for peer {id}");
      return Task.CompletedTask;
    }

    [PluginJsonRpcSubscription("disconnect")]
    public Task OnDisconnectAsync(string id)
    {
      _logger.LogInformation($"Received disconnect event for peer {id}");
      return Task.CompletedTask;
    }

    [PluginJsonRpcSubscription("invoice_payment")]
    public Task OnPaymentAsync(string label, string preimage, long msat)
    {
      _logger.LogInformation($"Received invoice_payment event for label {label}, preimage {preimage}, and" +
                             $"amount amount of {msat}");
      return Task.CompletedTask;
    }
  }
}
