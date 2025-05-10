import Toybox.Application;
import Toybox.Lang;
import Toybox.WatchUi;
import Toybox.Sensor;

class HeartRateAppApp extends Application.AppBase {
    var view; 
    function initialize() {
        AppBase.initialize();
    }

    // onStart() is called on application start up
    function onStart(state as Dictionary?) as Void {
        var view = new HeartRateAppView();
        var delegate = new HeartRateInputDelegate(view);
        WatchUi.pushView(view, delegate, WatchUi.SLIDE_IMMEDIATE);
    }

    // onStop() is called when your application is exiting
    function onStop(state as Dictionary?) as Void {
    }

    // Return the initial view of your application here
    function getInitialView() as [Views] or [Views, InputDelegates] {
        return [ new HeartRateAppView(), new HeartRateAppDelegate() ];
    }

}

function getApp() as HeartRateAppApp {
    return Application.getApp() as HeartRateAppApp;
}