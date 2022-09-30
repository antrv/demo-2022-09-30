using System;
using System.Globalization;
using Avalonia.Data.Converters;

namespace ClockApp.ViewModels;

public class TimeToAngleConverter : IValueConverter
{
    public object? Convert(object? value, Type targetType, object? parameter, CultureInfo culture)
    {
        const double hourAngle = 360.0 / 12;
        const double minuteAngle = 360.0 / 60;
        const double secondAngle = 360.0 / 60;

        if (parameter is string arrow && value is DateTime time)
        {
            switch (arrow)
            {
                case "h":
                    return time.Hour % 12 * hourAngle + time.Minute * hourAngle / 60.0;
                case "m":
                    return time.Minute * minuteAngle;
                case "s":
                    return time.Second * secondAngle;
            }
        }

        return value;
    }

    public object? ConvertBack(object? value, Type targetType, object? parameter, CultureInfo culture)
    {
        throw new NotImplementedException();
    }
}
