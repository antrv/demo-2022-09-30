using System;
using Avalonia.Threading;
using ReactiveUI;

namespace ClockApp.ViewModels;

public class MainViewModel : ViewModelBase
{
    private readonly DispatcherTimer _timer = new() { Interval = TimeSpan.FromMilliseconds(100) };
    private DateTime _time = DateTime.Now;

    public MainViewModel()
    {
        _timer.Tick += (_, _) => CurrentTime = DateTime.Now;
        _timer.Start();
    }
        
    public DateTime CurrentTime
    {
        get => _time;
        set
        {
            if (_time != value)
            {
                _time = value;
                this.RaisePropertyChanged();
            }
        }
    }
}
