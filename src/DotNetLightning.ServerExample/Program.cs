using System;
using System.Net;
using Microsoft.AspNetCore;
using Microsoft.AspNetCore.Connections;
using Microsoft.AspNetCore.Hosting;
using DotNetLightning.Server;

namespace DotNetLightning.ServerExample
{
    class Program
    {
        static void Main(string[] args)
        {
            CreateWebHostBuilder(args).Build().Run();
            Console.WriteLine("Hello World!");
        }

        public static IWebHostBuilder CreateWebHostBuilder(string[] args) =>
            WebHost.CreateDefaultBuilder(args)
                .ConfigureServices(services =>
                {
                    services.AddLightningP2P(new IPEndPoint(IPAddress.Loopback, 9735));
                })
                .UseKestrel(options => 
                {
                    options.Listen
                });


    }
}
