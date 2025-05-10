import Toybox.Lang;
import Toybox.WatchUi;

class HeartRateAppDelegate extends WatchUi.BehaviorDelegate {

    function initialize() {
        BehaviorDelegate.initialize();
    }

    function onMenu() as Boolean {
        WatchUi.pushView(new Rez.Menus.MainMenu(), new HeartRateAppMenuDelegate(), WatchUi.SLIDE_UP);
        return true;
    }

}