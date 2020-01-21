namespace DotNetLightning.DomainUtils.Types

type IState = interface end
type IStateData = interface end
type ICommand = interface end
type IEvent = interface end

/// Indicates an input from the user is not valid
type ValidationError = {
    ErrorMsg: string list
}
    with
    static member Create msg =
        {
            ErrorMsg = [msg]
        }

