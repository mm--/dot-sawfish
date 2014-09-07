#!/usr/bin/env python
# CEFtoolbar
# A dzen-like toolbar for my window manager
# This script is just a modified version of the "pygtk" example from cefpython

import ctypes, os, sys
libcef_so = os.path.join(os.path.dirname(os.path.abspath(__file__)),\
        'libcef.so')
if os.path.exists(libcef_so):
    # Import local module
    ctypes.CDLL(libcef_so, ctypes.RTLD_GLOBAL)
    if 0x02070000 <= sys.hexversion < 0x03000000:
        import cefpython_py27 as cefpython
    else:
        raise Exception("Unsupported python version: %s" % sys.version)
else:
    # Import from package
    from cefpython3 import cefpython

# import pygtk
# pygtk.require('2.0')
import gtk
import gobject
import re
import subprocess, select, sys
import atexit, signal

def GetApplicationPath(file=None):
    import re, os, platform
    # If file is None return current directory without trailing slash.
    if file is None:
        file = ""
    # Only when relative path.
    if not file.startswith("/") and not file.startswith("\\") and (
            not re.search(r"^[\w-]+:", file)):
        if hasattr(sys, "frozen"):
            path = os.path.dirname(sys.executable)
        elif "__file__" in globals():
            path = os.path.dirname(os.path.realpath(__file__))
        else:
            path = os.getcwd()
        path = path + os.sep + file
        if platform.system() == "Windows":
            path = re.sub(r"[/\\]+", re.escape(os.sep), path)
        path = re.sub(r"[/\\]+$", "", path)
        return path
    return str(file)

def ExceptHook(excType, excValue, traceObject):
    import traceback, os, time, codecs
    # This hook does the following: in case of exception write it to
    # the "error.log" file, display it to the console, shutdown CEF
    # and exit application immediately by ignoring "finally" (_exit()).
    errorMsg = "\n".join(traceback.format_exception(excType, excValue,
            traceObject))
    errorFile = GetApplicationPath("error.log")
    try:
        appEncoding = cefpython.g_applicationSettings["string_encoding"]
    except:
        appEncoding = "utf-8"
    if type(errorMsg) == bytes:
        errorMsg = errorMsg.decode(encoding=appEncoding, errors="replace")
    try:
        with codecs.open(errorFile, mode="a", encoding=appEncoding) as fp:
            fp.write("\n[%s] %s\n" % (
                    time.strftime("%Y-%m-%d %H:%M:%S"), errorMsg))
    except:
        print("[pygtk_.py]: WARNING: failed writing to error file: %s" % (
                errorFile))
    # Convert error message to ascii before printing, otherwise
    # you may get error like this:
    # | UnicodeEncodeError: 'charmap' codec can't encode characters
    errorMsg = errorMsg.encode("ascii", errors="replace")
    errorMsg = errorMsg.decode("ascii", errors="replace")
    print("\n"+errorMsg+"\n")
    cefpython.QuitMessageLoop()
    cefpython.Shutdown()
    os._exit(1)

class CEFToolbar:
    mainWindow = None
    container = None
    browser = None
    exiting = None
    searchEntry = None
    vbox = None

    def __init__(self):
        self.mainWindow = gtk.Window(gtk.WINDOW_TOPLEVEL)
        self.mainWindow.connect('destroy', self.OnExit)
        self.mainWindow.set_decorated(False)
        self.mainWindow.stick()
        self.mainWindow.set_size_request(width=1450, height=16)
        self.mainWindow.set_title('ceftoolbar')
        self.mainWindow.realize()

        self.vbox = gtk.VBox(False, 0)
        # self.vbox.pack_start(self.CreateMenu(), False, False, 0)
        self.mainWindow.add(self.vbox)

        m = re.search("GtkVBox at 0x(\w+)", str(self.vbox))
        hexID = m.group(1)
        windowID = int(hexID, 16)

        windowInfo = cefpython.WindowInfo()
        windowInfo.SetAsChild(windowID)
        # Linux requires adding "file://" for local files,
        # otherwise /home/some will be replaced as http://home/some
        self.browser = cefpython.CreateBrowserSync(
            windowInfo,
            browserSettings={},
            navigateUrl="file://"+GetApplicationPath("ceftoolbar.html"))

        jsBindings = cefpython.JavascriptBindings(bindToFrames=False, bindToPopups=True)
        jsBindings.SetObject("external", JavascriptExternal(self.browser))
        self.browser.SetJavascriptBindings(jsBindings)

        self.myTracker = PipeTracker(self.browser)

        self.vbox.show()
        self.mainWindow.show()
        gobject.timeout_add(1, self.OnTimer)
        gobject.timeout_add(1, self.OnPipeTimer)

    def OnWidgetClick(self, widget, data):
        self.mainWindow.get_window().focus()

    def OnTimer(self):
        if self.exiting:
            return False
        cefpython.MessageLoopWork()
        return True

    def OnPipeTimer(self):
        if self.exiting:
            return False
        self.myTracker.handleEvents()
        return True

    def OnFocusIn(self, widget, data):
        # This function is currently not called by any of code,
        # but if you would like for browser to have automatic focus
        # add such line:
        # self.mainWindow.connect('focus-in-event', self.OnFocusIn)
        self.browser.SetFocus(True)

    def OnExit(self, widget, data=None):
        self.exiting = True
        gtk.main_quit()

class JavascriptExternal:
    mainBrowser = None
    stringVisitor = None
    
    def __init__(self, mainBrowser):
        self.mainBrowser = mainBrowser

    def GoBack(self):
        self.mainBrowser.GoBack()

    def GoForward(self):
        self.mainBrowser.GoForward()

    def CreateAnotherBrowser(self):
        frame = MainFrame()
        frame.Show()

    def Print(self, message):
        print(message)

    def TestAllTypes(self, *args):
        print(args)

    def ExecuteFunction(self, *args):
        self.mainBrowser.GetMainFrame().ExecuteFunction(*args)

    def TestJSCallback(self, jsCallback):
        global hey
        print("jsCallback.GetFunctionName() = %s" % jsCallback.GetFunctionName())
        print("jsCallback.GetFrame().GetIdentifier() = %s" % \
                jsCallback.GetFrame().GetIdentifier())
        hey = hey + "BB"
        jsCallback.Call("This message was sent from python using js callback" + hey)

    def updateCallback(self, jsCallback):
        global remember
        jsCallback.Call(remember)

    def TestJSCallbackComplexArguments(self, jsObject):
        jsCallback = jsObject["myCallback"];
        jsCallback.Call(1, None, 2.14, "string", ["list", ["nested list", \
                {"nested object":None}]], \
                {"nested list next":[{"deeply nested object":1}]})

    def TestPythonCallback(self, jsCallback):
        jsCallback.Call(self.PyCallback)

    def PyCallback(self, *args):
        message = "PyCallback() was executed successfully! Arguments: %s" \
                % str(args)
        print(message)
        self.mainBrowser.GetMainFrame().ExecuteJavascript(
                "window.alert(\"%s\")" % message)

    def GetSource(self):
        # Must keep a strong reference to the StringVisitor object
        # during the visit.
        self.stringVisitor = StringVisitor()
        self.mainBrowser.GetMainFrame().GetSource(self.stringVisitor)

    def GetText(self):
        # Must keep a strong reference to the StringVisitor object
        # during the visit.
        self.stringVisitor = StringVisitor()
        self.mainBrowser.GetMainFrame().GetText(self.stringVisitor)

    # -------------------------------------------------------------------------
    # Cookies
    # -------------------------------------------------------------------------
    cookieVisitor = None

    def VisitAllCookies(self):
        # Need to keep the reference alive.
        self.cookieVisitor = CookieVisitor()
        cookieManager = self.mainBrowser.GetUserData("cookieManager")
        if not cookieManager:
            print("\nCookie manager not yet created! Visit http website first")
            return
        cookieManager.VisitAllCookies(self.cookieVisitor)

    def VisitUrlCookies(self):
        # Need to keep the reference alive.
        self.cookieVisitor = CookieVisitor()
        cookieManager = self.mainBrowser.GetUserData("cookieManager")
        if not cookieManager:
            print("\nCookie manager not yet created! Visit http website first")
            return
        cookieManager.VisitUrlCookies(
            "http://www.html-kit.com/tools/cookietester/",
            False, self.cookieVisitor)
        # .www.html-kit.com

    def SetCookie(self):
        cookieManager = self.mainBrowser.GetUserData("cookieManager")
        if not cookieManager:
            print("\nCookie manager not yet created! Visit http website first")
            return
        cookie = cefpython.Cookie()
        cookie.SetName("Created_Via_Python")
        cookie.SetValue("yeah really")
        cookieManager.SetCookie("http://www.html-kit.com/tools/cookietester/",
                cookie)
        print("\nCookie created! Visit html-kit cookietester to see it")

    def DeleteCookies(self):
        cookieManager = self.mainBrowser.GetUserData("cookieManager")
        if not cookieManager:
            print("\nCookie manager not yet created! Visit http website first")
            return
        cookieManager.DeleteCookies(
                "http://www.html-kit.com/tools/cookietester/",
                "Created_Via_Python")
        print("\nCookie deleted! Visit html-kit cookietester to see the result")

def kill_process(pid):
    print "Trying to kill ", pid, " ", os.getpgid(pid)
    os.killpg(os.getpgid(pid), signal.SIGTERM)

class PipeTracker:
    def __init__(self, mainBrowser):
        self.mainBrowser = mainBrowser
        process_scripts = [['/home/jm3/.sawfish/scripts/conky-ceftoolbar.sh'], 
                           ['/home/jm3/.sawfish/scripts/replace-listen-fifo.sh', '0.1'],
                           ['/home/jm3/.sawfish/scripts/log-alert.sh']]
        processes = [subprocess.Popen(x,stdout=subprocess.PIPE,stderr=subprocess.PIPE, preexec_fn=os.setsid)
                     for x in process_scripts]
        self.fdtoprocess = dict([(x.stdout.fileno(), x.stdout) for x in processes])
        self.poll = select.poll()
        for x in processes:
            atexit.register(kill_process, x.pid)
            self.poll.register(x.stdout,select.POLLIN | select.POLLHUP)
        self.pollc = len(processes)


    def handleEvents(self):
        if self.pollc > 0: 
            events = self.poll.poll(100)
        for event in events:
            (rfd,event) = event
            if event & select.POLLIN:
                line = self.fdtoprocess[rfd].readline().rstrip()
                if line_handler(line, self.mainBrowser.GetMainFrame()):
                    self.mainBrowser.GetMainFrame().ExecuteFunction("callUpdate")
                    self.mainBrowser.GetMainFrame().ExecuteFunction("nextPoint", remember["CPU"])
            if event & select.POLLHUP:
                self.poll.unregister(rfd)
                self.pollc = self.pollc - 1


remember = {}

def line_handler(line, mainframe):
    """Take a line. If it's not a REPLACE then remember it. Output to
stdout if it's a FLUSH"""
    m = re.match(r"^(.*?):(.*)$", line)
    if m:
        command, rest = m.group(1,2)
        if command == "UPDATE":
            return True
        elif command == "ALERT":
            mainframe.ExecuteFunction("logAlert", rest)
        elif command == "DESK":
            mainframe.ExecuteFunction("deskSet", rest)
        else:
            remember[command] = rest
    return False

if __name__ == '__main__':
    version = '.'.join(map(str, list(gtk.gtk_version)))
    print('[pygtk_.py] GTK version: %s' % version)

    # Intercept python exceptions. Exit app immediately when exception
    # happens on any of the threads.
    sys.excepthook = ExceptHook

    # Application settings
    settings = {
        "debug": False, # cefpython debug messages in console and in log_file
        "log_severity": cefpython.LOGSEVERITY_INFO, # LOGSEVERITY_VERBOSE
        "log_file": GetApplicationPath("debug.log"), # Set to "" to disable
        "release_dcheck_enabled": True, # Enable only when debugging
        # This directories must be set on Linux
        "locales_dir_path": cefpython.GetModuleDirectory()+"/locales",
        "resources_dir_path": cefpython.GetModuleDirectory(),
        "browser_subprocess_path": "%s/%s" % (
            cefpython.GetModuleDirectory(), "subprocess"),
    }

    cefpython.Initialize(settings)

    gobject.threads_init() # Timer for the message loop
    CEFToolbar()
    gtk.main()

    cefpython.Shutdown()
