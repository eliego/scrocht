using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Imaging;
using System.Windows.Forms;
namespace Scrochter
{
    class Program
    {
        static void Main(string[] args)
        {
            string BrowserExec = args[0];
            int XRes = int.Parse(args[1]);
            int YRes = int.Parse(args[2]);
            string Url = args[3];
            string Filename = args[4];

            try
            {
                // Change resolution
                changeResolution(XRes, YRes);

                // Start browser
                Process p = new Process();
                p.StartInfo.FileName = BrowserExec;
                p.StartInfo.Arguments = Url;
                p.Start();

                // Wait for window to open, and then maximize and bring to front
                System.Threading.Thread.Sleep(500);
                maximizeFront(p.MainWindowHandle);

                // Wait for page to load
                System.Threading.Thread.Sleep(5000);

                // Take screenshot
                saveScreenshot(Filename);

                // Exit browser
                p.CloseMainWindow();
                System.Threading.Thread.Sleep(500); // Is this necessary!?
                if (!p.HasExited)
                    p.Kill();
            }
            catch (Exception e)
            {
                ; // Do nothing, ugly but necessary
            }
        }

        private static void maximizeFront(IntPtr hWnd)
        {
            User32.SendMessage(hWnd, User32.WM_SYSCOMMAND, new IntPtr(User32.SC_MAXIMIZE), IntPtr.Zero);
            User32.SetForegroundWindow(hWnd);
            User32.SetActiveWindow(hWnd);
        }

        private static void saveScreenshot(string Filename)
        {
            Bitmap bmp;
            Graphics gfx;
            bmp = new Bitmap(Screen.PrimaryScreen.Bounds.Width, Screen.PrimaryScreen.Bounds.Height, PixelFormat.Format32bppArgb);
            gfx = Graphics.FromImage(bmp);
            gfx.CopyFromScreen(Screen.PrimaryScreen.Bounds.X, Screen.PrimaryScreen.Bounds.Y, 0, 0, Screen.PrimaryScreen.Bounds.Size, CopyPixelOperation.SourceCopy);
            bmp.Save(Filename, ImageFormat.Jpeg);
        }

        private static void changeResolution(int X, int Y)
        {
            DEVMODE dm = new DEVMODE();
            dm.dmDeviceName = new String(new char[32]);
            dm.dmFormName = new String(new char[32]);
            dm.dmSize = (short)Marshal.SizeOf(dm);

            User32.EnumDisplaySettings(null, User32.ENUM_CURRENT_SETTINGS, ref dm);

            dm.dmPelsWidth = X;
            dm.dmPelsHeight = Y;

            User32.ChangeDisplaySettings(ref dm, User32.CDS_UPDATEREGSITRY);
        }
    }
}
