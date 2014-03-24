#!/usr/bin/env python
# An example of embedding CEF browser in wxPython on Linux.

# Important: 
#   On Linux importing the cefpython module must be 
#   the very first in your application. This is because CEF makes 
#   a global tcmalloc hook for memory allocation/deallocation. 
#   See Issue 73 that is to provide CEF builds with tcmalloc disabled:
#   https://code.google.com/p/cefpython/issues/detail?id=73

import ctypes, os, sys
libcef_so = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'libcef.so')
if os.path.exists(libcef_so):
    # Import local module
    ctypes.CDLL(libcef_so, ctypes.RTLD_GLOBAL)
    if 0x02070000 <= sys.hexversion < 0x03000000:
        import cefpython_py27 as cefpython
    else:
        raise Exception("Unsupported python version: %s" % sys.version)
else:
    # Import from package
    print "FROM PACKAGE"
    from cefpython3 import cefpython

import wx
import time
import re
import uuid
import platform
import gtk                      # For sticky setting
import subprocess, select, sys

# Which method to use for message loop processing.
#   EVT_IDLE - wx application has priority (default)
#   EVT_TIMER - cef browser has priority
# It seems that Flash content behaves better when using a timer.
# IMPORTANT! On Linux EVT_IDLE does not work, the events seems to
# be propagated only when you move your mouse, which is not the
# expected behavior, it is recommended to use EVT_TIMER on Linux,
# so set this value to False.
USE_EVT_IDLE = False

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
    # and exit application immediately by ignoring "finally" (os._exit()).
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
        print("cefpython: WARNING: failed writing to error file: %s" % (
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

def set_sticky(frame):
    gdkwin = gtk.gdk.window_lookup(frame.GetHandle())
    win = gdkwin.get_user_data() # http://library.gnome.org/devel/gdk/unstable/gdk-Windows.html#gdk-window-get-user-data
    while not isinstance(win, gtk.Window):
        win = win.get_parent()
    print "AYEALEFAA", win.window.xid
    win.stick()

def test_two(frame):
    print "WIDGY", frame.GetGtkWidget()
    widge = frame.GetGtkWidget()
    # widge.set_type_hint(gtk.gdk.WINDOW_TYPE_HINT_DOCK)
    # gdkwin = gtk.gdk.window_lookup(frame.GetHandle())
    # win = gdkwin.get_user_data() # http://library.gnome.org/devel/gdk/unstable/gdk-Windows.html#gdk-window-get-user-data
    # while not isinstance(win, gtk.Window):
    #     win = win.get_parent()
    # win.stick()
    # print win.get_skip_pager_hint()
    # win.set_skip_pager_hint(True)
    # win.set_type_hint(gtk.gdk.WINDOW_TYPE_HINT_DOCK)
    # print win.get_skip_pager_hint()


class MainFrame(wx.Frame):
    browser = None
    mainPanel = None
    width = 1450
    height = 16
    def __init__(self):
        wx.Frame.__init__(self, parent=None, id=wx.ID_ANY,
                          pos = (9000, 9000),
                          style= wx.BORDER_NONE |  wx.FRAME_NO_TASKBAR,
                          title='ceftoolbar', size=(self.width,self.height))

        # Cannot attach browser to the main frame as this will cause
        # the menu not to work.
        # --
        # You also have to set the wx.WANTS_CHARS style for
        # all parent panels/controls, if it's deeply embedded.
        # self.mainPanel = wx.Panel(self, style=wx.WANTS_CHARS)
        # self.mainPanel = wx.Panel(self, style=wx.WANTS_CHARS)
        sizer = wx.BoxSizer(wx.VERTICAL)
        self.mainPanel = wx.Panel(self, size=(self.width,self.height+2), style=wx.BORDER_RAISED)
        # self.mainPanel = wx.Panel(self, size=(500,500), style=wx.BORDER_RAISED)
        sizer.Add(self.mainPanel, flag = wx.EXPAND | wx.ALL, proportion=0, border=-1)
        self.SetSizer(sizer)

        windowInfo = cefpython.WindowInfo()
        windowInfo.SetAsChild(self.mainPanel.GetGtkWidget())

        print self.GetGtkWidget()
        gdkwin = gtk.gdk.window_lookup(self.GetId())
        print "WHOAH", self.mainPanel.GetGtkWidget()

        # Linux requires adding "file://" for local files,
        # otherwise /home/some will be replaced as http://home/some
        self.browser = cefpython.CreateBrowserSync(
            windowInfo,
            # If there are problems with Flash you can disable it here,
            # by disabling all plugins.
            browserSettings={"plugins_disabled": False},
            navigateUrl="file://"+GetApplicationPath("ceftoolbar.html"))

        clientHandler = ClientHandler()
        self.browser.SetClientHandler(clientHandler)
        cefpython.SetGlobalClientCallback("OnCertificateError",
                clientHandler._OnCertificateError)
        cefpython.SetGlobalClientCallback("OnBeforePluginLoad",
                clientHandler._OnBeforePluginLoad)

        jsBindings = cefpython.JavascriptBindings(
            bindToFrames=False, bindToPopups=True)
        jsBindings.SetFunction("PyPrint", PyPrint)
        jsBindings.SetProperty("pyProperty", "This was set in Python")
        jsBindings.SetProperty("pyConfig", ["This was set in Python",
                {"name": "Nested dictionary", "isNested": True},
                [1,"2", None]])
        jsBindings.SetObject("external", JavascriptExternal(self.browser))
        self.browser.SetJavascriptBindings(jsBindings)

        self.Bind(wx.EVT_CLOSE, self.OnClose)
        if USE_EVT_IDLE:
            # Bind EVT_IDLE only for the main application frame.
            self.Bind(wx.EVT_IDLE, self.OnIdle)

    def OnClose(self, event):
        self.browser.CloseBrowser()
        self.Destroy()

    def OnIdle(self, event):
        cefpython.MessageLoopWork()

def PyPrint(message):
    print(message)

hey = "AA"
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

class StringVisitor:
    def Visit(self, string):
        print("\nStringVisitor.Visit(): string:")
        print("--------------------------------")
        print(string)
        print("--------------------------------")

class CookieVisitor:
    def Visit(self, cookie, count, total, deleteCookie):
        if count == 0:
            print("\nCookieVisitor.Visit(): total cookies: %s" % total)
        print("\nCookieVisitor.Visit(): cookie:")
        print(cookie.Get())
        # True to continue visiting cookies
        return True

class ClientHandler:

    # -------------------------------------------------------------------------
    # DisplayHandler
    # -------------------------------------------------------------------------

    def OnLoadingStateChange(self, browser, isLoading, canGoBack, 
            canGoForward):
        print("DisplayHandler::OnLoadingStateChange()")
        print("isLoading = %s, canGoBack = %s, canGoForward = %s" \
                % (isLoading, canGoBack, canGoForward))

    def OnAddressChange(self, browser, frame, url):
        print("DisplayHandler::OnAddressChange()")
        print("url = %s" % url)

    def OnTitleChange(self, browser, title):
        print("DisplayHandler::OnTitleChange()")
        print("title = %s" % title)

    def OnTooltip(self, browser, textOut):
        # OnTooltip not yet implemented (both Linux and Windows), 
        # will be fixed in next CEF release, see Issue 783:
        # https://code.google.com/p/chromiumembedded/issues/detail?id=783
        print("DisplayHandler::OnTooltip()")
        print("text = %s" % textOut[0])

    statusMessageCount = 0
    def OnStatusMessage(self, browser, value):
        if not value:
            # Do not notify in the console about empty statuses.
            return
        self.statusMessageCount += 1
        if self.statusMessageCount > 3:
            # Do not spam too much.
            return
        print("DisplayHandler::OnStatusMessage()")
        print("value = %s" % value)

    def OnConsoleMessage(self, browser, message, source, line):
        print("DisplayHandler::OnConsoleMessage()")
        print("message = %s" % message)
        print("source = %s" % source)
        print("line = %s" % line)

    # -------------------------------------------------------------------------
    # KeyboardHandler
    # -------------------------------------------------------------------------

    def OnPreKeyEvent(self, browser, event, eventHandle, 
            isKeyboardShortcutOut):
        print("KeyboardHandler::OnPreKeyEvent()")

    def OnKeyEvent(self, browser, event, eventHandle):
        print("KeyboardHandler::OnKeyEvent()")
        print("native_key_code = %s" % event["native_key_code"])
        if platform.system() == "Linux":
            # F5 = 71
            if event["native_key_code"] == 71:
                print("F5 pressed! Reloading page..")
                browser.ReloadIgnoreCache()

    # -------------------------------------------------------------------------
    # RequestHandler
    # -------------------------------------------------------------------------

    def OnBeforeBrowse(self, browser, frame, request, isRedirect):
        print("RequestHandler::OnBeforeBrowse()")
        print("url = %s" % request.GetUrl()[:70])
        return False

    def OnBeforeResourceLoad(self, browser, frame, request):
        print("RequestHandler::OnBeforeResourceLoad()")
        print("url = %s" % request.GetUrl()[:70])
        return False

    def OnResourceRedirect(self, browser, frame, oldUrl, newUrlOut):
        print("RequestHandler::OnResourceRedirect()")
        print("old url = %s" % oldUrl[:70])
        print("new url = %s" % newUrlOut[0][:70])

    def GetAuthCredentials(self, browser, frame, isProxy, host, port, realm,
            scheme, callback):
        print("RequestHandler::GetAuthCredentials()")
        print("host = %s" % host)
        print("realm = %s" % realm)
        callback.Continue(username="test", password="test")
        return True

    def OnQuotaRequest(self, browser, originUrl, newSize, callback):
        print("RequestHandler::OnQuotaRequest()")
        print("origin url = %s" % originUrl)
        print("new size = %s" % newSize)
        callback.Continue(True)
        return True

    def GetCookieManager(self, browser, mainUrl):
        # Create unique cookie manager for each browser.
        # --
        # Buggy implementation in CEF, reported here:
        # https://code.google.com/p/chromiumembedded/issues/detail?id=1043
        cookieManager = browser.GetUserData("cookieManager")
        if cookieManager:
            return cookieManager
        else:
            cookieManager = cefpython.CookieManager.CreateManager("")
            browser.SetUserData("cookieManager", cookieManager)
            return cookieManager

    def OnProtocolExecution(self, browser, url, allowExecutionOut):
        # There's no default implementation for OnProtocolExecution on Linux,
        # you have to make OS system call on your own. You probably also need
        # to use LoadHandler::OnLoadError() when implementing this on Linux.
        print("RequestHandler::OnProtocolExecution()")
        print("url = %s" % url)
        if url.startswith("magnet:"):
            print("Magnet link allowed!")
            allowExecutionOut[0] = True

    def _OnBeforePluginLoad(self, browser, url, policyUrl, info):
        # Plugins are loaded on demand, only when website requires it,
        # the same plugin may be called multiple times.
        print("RequestHandler::OnBeforePluginLoad()")
        print("url = %s" % url)
        print("policy url = %s" % policyUrl)
        print("info.GetName() = %s" % info.GetName())
        print("info.GetPath() = %s" % info.GetPath())
        print("info.GetVersion() = %s" % info.GetVersion())
        print("info.GetDescription() = %s" % info.GetDescription())
        # False to allow, True to block plugin.
        return False

    def _OnCertificateError(self, certError, requestUrl, callback):
        print("RequestHandler::OnCertificateError()")
        print("certError = %s" % certError)
        print("requestUrl = %s" % requestUrl)
        if requestUrl.startswith(
                "https://sage.math.washington.edu:8091/do-not-allow"):
            print("Not allowed!")
            return False
        if requestUrl.startswith(
                "https://sage.math.washington.edu:8091/hudson/job/"):
            print("Allowed!")
            callback.Continue(True)
            return True
        return False

    # -------------------------------------------------------------------------
    # LoadHandler
    # -------------------------------------------------------------------------
    
    def OnLoadStart(self, browser, frame):
        print("LoadHandler::OnLoadStart()")
        print("frame url = %s" % frame.GetUrl()[:70])

    def OnLoadEnd(self, browser, frame, httpStatusCode):
        print("LoadHandler::OnLoadEnd()")
        print("frame url = %s" % frame.GetUrl()[:70])
        # For file:// urls the status code = 0
        print("http status code = %s" % httpStatusCode)

    def OnLoadError(self, browser, frame, errorCode, errorTextList, failedUrl):
        print("LoadHandler::OnLoadError()")
        print("frame url = %s" % frame.GetUrl()[:70])
        print("error code = %s" % errorCode)
        print("error text = %s" % errorTextList[0])
        print("failed url = %s" % failedUrl)
        # customErrorMessage = "My custom error message!"
        # frame.LoadUrl("data:text/html,%s" % customErrorMessage)

    def OnRendererProcessTerminated(self, browser, status):
        print("LoadHandler::OnRendererProcessTerminated()")
        statuses = {
            cefpython.TS_ABNORMAL_TERMINATION: "TS_ABNORMAL_TERMINATION",
            cefpython.TS_PROCESS_WAS_KILLED: "TS_PROCESS_WAS_KILLED",
            cefpython.TS_PROCESS_CRASHED: "TS_PROCESS_CRASHED"
        }
        statusName = "Unknown"
        if status in statuses:
            statusName = statuses[status]
        print("status = %s" % statusName)

    def OnPluginCrashed(self, browser, pluginPath):
        print("LoadHandler::OnPluginCrashed()")
        print("plugin path = %s" % pluginPath)

    # -------------------------------------------------------------------------
    # LifespanHandler
    # -------------------------------------------------------------------------

    # Empty place-holders: popupFeatures, windowInfo, client, browserSettings.
    def OnBeforePopup(self, browser, frame, targetUrl, targetFrameName,
            popupFeatures, windowInfo, client, browserSettings, noJavascriptAccess):
        print("LifespanHandler::OnBeforePopup()")
        print("targetUrl = %s" % targetUrl)
        allowPopups = True
        return not allowPopups

class PipeTracker:
    def __init__(self, mainBrowser):
        self.mainBrowser = mainBrowser
        conky = subprocess.Popen(['/home/jm3/.sawfish/scripts/conky-ceftoolbar.sh'],stdout=subprocess.PIPE,stderr=subprocess.PIPE)
        fifo = subprocess.Popen(['/home/jm3/.sawfish/scripts/replace-listen-fifo.sh', '0.1'],stdout=subprocess.PIPE,stderr=subprocess.PIPE)
        processes = [conky, fifo]
        self.fdtoprocess = dict([(x.stdout.fileno(), x.stdout) for x in processes])
        self.poll = select.poll()
        for x in processes:
            self.poll.register(x.stdout,select.POLLIN | select.POLLHUP)
        self.pollc = len(processes)


    def handleEvents(self):
        if self.pollc > 0: 
            events = self.poll.poll(100)
        for event in events:
            (rfd,event) = event
            if event & select.POLLIN:
                line = self.fdtoprocess[rfd].readline().rstrip()
                if line_handler(line):
                    self.mainBrowser.GetMainFrame().ExecuteFunction("callUpdate")
                    self.mainBrowser.GetMainFrame().ExecuteFunction("nextPoint", remember["CPU"])
            if event & select.POLLHUP:
                self.poll.unregister(rfd)
                self.pollc = self.pollc - 1


remember = {}

def line_handler(line):
    """Take a line. If it's not a REPLACE then remember it. Output to
stdout if it's a FLUSH"""
    m = re.match(r"^(.*?):(.*)$", line)
    if m:
        command, rest = m.group(1,2)
        if command == "UPDATE":
            return True
        else:
            remember[command] = rest
    return False


class MyApp(wx.App):
    timer = None
    timerID = 1
    timerCount = 0

    def OnInit(self):
        frame = MainFrame()
        self.myTracker = PipeTracker(frame.browser)
        if not USE_EVT_IDLE:
            self.CreateTimer()
        self.SetTopWindow(frame)
        test_two(frame)
        frame.Show()
        # frame.MoveXY(000,000)
        test_two(frame)
        set_sticky(frame)
        return True

    def CreateTimer(self):
        # See "Making a render loop":
        # http://wiki.wxwidgets.org/Making_a_render_loop
        # Another approach is to use EVT_IDLE in MainFrame,
        # see which one fits you better.
        self.timer = wx.Timer(self, self.timerID)
        self.timer.Start(2) # 10ms
        wx.EVT_TIMER(self, self.timerID, self.OnTimer)

    def OnTimer(self, event):
        self.timerCount += 1
        # print("wxpython.py: OnTimer() %d" % self.timerCount)
        self.myTracker.handleEvents()
        cefpython.MessageLoopWork()

    def OnExit(self):
        # When app.MainLoop() returns, MessageLoopWork() should
        # not be called anymore.
        if not USE_EVT_IDLE:
            self.timer.Stop()

if __name__ == '__main__':
    sys.excepthook = ExceptHook
    cefpython.g_debug = False
    cefpython.g_debugFile = GetApplicationPath("debug.log")
    settings = {
        "log_severity": cefpython.LOGSEVERITY_INFO, # LOGSEVERITY_VERBOSE
        "log_file": GetApplicationPath("debug.log"), # Set to "" to disable.
        "release_dcheck_enabled": True, # Enable only when debugging.
        # This directories must be set on Linux
        "locales_dir_path": cefpython.GetModuleDirectory()+"/locales",
        "resources_dir_path": cefpython.GetModuleDirectory(),
        "browser_subprocess_path": "%s/%s" % (
            cefpython.GetModuleDirectory(), "subprocess")
    }
    # print("browser_subprocess_path="+settings["browser_subprocess_path"])
    cefpython.Initialize(settings)
    print('wx.version=%s' % wx.version())
    app = MyApp(False)
    app.MainLoop()
    # Let wx.App destructor do the cleanup before calling cefpython.Shutdown().
    del app
    cefpython.Shutdown()
