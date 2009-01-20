using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
namespace Scrochter
{
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
    public struct DEVMODE
    {
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]
        public string dmDeviceName;

        public short dmSpecVersion;
        public short dmDriverVersion;
        public short dmSize;
        public short dmDriverExtra;
        public int dmFields;
        public int dmPositionX;
        public int dmPositionY;
        public int dmDisplayOrientation;
        public int dmDisplayFixedOutput;
        public short dmColor;
        public short dmDuplex;
        public short dmYResolution;
        public short dmTTOption;
        public short dmCollate;

        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]
        public string dmFormName;

        public short dmLogPixels;
        public short dmBitsPerPel;
        public int dmPelsWidth;
        public int dmPelsHeight;
        public int dmDisplayFlags;
        public int dmDisplayFrequency;
        public int dmICMMethod;
        public int dmICMIntent;
        public int dmMediaType;
        public int dmDitherType;
        public int dmReserved1;
        public int dmReserved2;
        public int dmPanningWidth;
        public int dmPanningHeight;
    };
    class User32
    {
        [DllImport("user32.dll", CharSet=CharSet.Auto, SetLastError=true)]
        public static extern IntPtr SendMessage(IntPtr hwnd, uint Msg, IntPtr wParam, IntPtr lParam);

        [DllImport("user32.dll")]
        public static extern int EnumDisplaySettings(string deviceName, int modeNum, ref DEVMODE devMode);

        [DllImport("user32.dll")]
        public static extern int ChangeDisplaySettings(ref DEVMODE devMode, int flags);

        [DllImport("user32.dll")]
        public static extern int SetActiveWindow(IntPtr hWnd);

        [DllImport("user32.dll")]
        public static extern bool SetForegroundWindow(IntPtr hWnd);

        public const int ENUM_CURRENT_SETTINGS = -1;
        public const int CDS_UPDATEREGSITRY = 0x01;
        public const int CDS_TEST = 0x02;
        public const int DISP_CHANGE_SUCCESSFUL = 0;
        public const int DISP_CHANGE_RESTART = 1;
        public const int DISP_CHANGE_FAILED = -1;

        public const int WM_QUIT = 0x0012;
        public const int WM_SYSCOMMAND = 0x0112;

        public const int SC_FIRST = 0xF000;
        public const int SC_MINIMIZE = SC_FIRST + 0x20; //	Minimizes the window.
        public const int SC_MAXIMIZE = SC_FIRST + 0x30; //	Maximizes the window.
    }
}
