using Avalonia.Web.Blazor;

namespace ClockApp.Web;

public partial class App
{
    protected override void OnParametersSet()
    {
        base.OnParametersSet();
        
        WebAppBuilder.Configure<ClockApp.App>()
            .SetupWithSingleViewLifetime();
    }
}