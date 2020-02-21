namespace DotNetLightning.Infrastructure.Interfaces

open DotNetLightning.Infrastructure
open System.Threading.Tasks

type IChannelEventStream =
    abstract member SetEventsAsync: events: ChannelEventWithContext seq -> Task
