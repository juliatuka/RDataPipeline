using Toybox.Application;
using Toybox.Graphics;
using Toybox.Lang;
using Toybox.Sensor;
using Toybox.System;
using Toybox.WatchUi;

class HeartRateInputDelegate extends WatchUi.InputDelegate {
    var view1;

    function initialize(view) {
        InputDelegate.initialize();
        view1 = view;
    }

    function onKey(keyEvent) {
        if (keyEvent.getKey() == WatchUi.KEY_ESC) {
            return true;
        }
        else{
            return false;
        }
    }
}