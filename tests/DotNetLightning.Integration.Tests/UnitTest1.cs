using System;
using DotNetLightning.Server;
using Xunit;
using Microsoft.AspNetCore.Mvc.Testing;

namespace DotNetLightning.Integration.Tests
{
    public class BasicControllerTest : IClassFixture<WebApplicationFactory<Startup>>
    {
        private readonly WebApplicationFactory<Startup> _factory;
        public BasicControllerTest(WebApplicationFactory<Startup> factory)
        {
            _factory = factory;
        }
        
        [Fact]
        public async void Test1()
        {
            // var client = _factory.CreateClient();
            // var info = await client.GetAsync("/api/v1/info");
            // info.EnsureSuccessStatusCode();
        }
    }
}
