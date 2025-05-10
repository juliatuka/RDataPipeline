import Toybox.Graphics;
import Toybox.WatchUi;
import Toybox.Sensor;
import Toybox.Communications;
import Toybox.ActivityMonitor;
import Toybox.Lang;
import Toybox.System;
import Toybox.Time;

class HeartRateAppView extends WatchUi.View {
  var heartRate = null;
  var pulseOx = null;
  var stressScore = null;
  var respirationRate = null;
  var bodyBattery = null;
  var ibiData = [];
  var accel = [];

  var sensorListenerCallBackMethod;
  // var respondData = "...";
  // var respondCode = "000";

  function initialize() {
    View.initialize();
  }

  // Load your resources here
  function onLayout(dc as Dc) as Void {
    var width = dc.getWidth();

    dc.setColor(Graphics.COLOR_WHITE, Graphics.COLOR_BLACK);
    dc.clear();

    System.println("Rendering screen...");
    dc.drawText(
      width / 2,
      10,
      Graphics.FONT_AUX2,
      "Heart " + heartRate,
      Graphics.TEXT_JUSTIFY_CENTER
    );

    dc.drawText(
      width / 2,
      60,
      Graphics.FONT_AUX2,
      "PulseOx " + pulseOx,
      Graphics.TEXT_JUSTIFY_CENTER
    );

    dc.drawText(
      width / 2,
      110,
      Graphics.FONT_AUX2,
      "Stress Score: " + stressScore,
      Graphics.TEXT_JUSTIFY_CENTER
    );

    dc.drawText(
      width / 2,
      160,
      Graphics.FONT_AUX2,
      "Respiration Rate: " + respirationRate,
      Graphics.TEXT_JUSTIFY_CENTER
    );

    dc.drawText(
      width / 2,
      210,
      Graphics.FONT_AUX2,
      "Body Battery: " + bodyBattery,
      Graphics.TEXT_JUSTIFY_CENTER
    );

    dc.drawText(
      width / 2,
      260,
      Graphics.FONT_AUX2,
      "IBI: " + formatIBI(),
      Graphics.TEXT_JUSTIFY_CENTER
    );

    dc.drawText(
      width / 2,
      310,
      Graphics.FONT_AUX2,
      "Accel: " + formatAccel(),
      Graphics.TEXT_JUSTIFY_CENTER
    );

    // ResposeCode / Data
    // dc.drawText(
    //   width / 2,
    //   200,
    //   Graphics.FONT_AUX2,
    //   respondData,
    //   Graphics.TEXT_JUSTIFY_CENTER
    // );

    // dc.drawText(
    //   width / 2,
    //   240,
    //   Graphics.FONT_AUX2,
    //   respondCode,
    //   Graphics.TEXT_JUSTIFY_CENTER
    // );
  }

  function formatIBI() as String {
    if (ibiData == null || ibiData.size() == 0) {
      return "No Data";
    }
    var output = "";
    for (var i = 0; i < ibiData.size(); i++) {
      output += ibiData[i].toString() + " ";
    }
    return output;
  }

  function formatAccel() as String {
    if (accel == null || accel.size() == 0) {
      return "No Data";
    }
    var output = "";
    for (var i = 0; i < accel.size(); i++) {
      output += accel[i].toString() + " ";
    }
    return output;
  }

  // Called when this View is brought to the foreground. Restore
  // the state of this View and prepare it to be shown. This includes
  // loading resources into memory.
  function onShow() as Void {
    System.println("App started");

    Sensor.setEnabledSensors([
      Sensor.SENSOR_HEARTRATE,
      Sensor.SENSOR_PULSE_OXIMETRY,
    ]);
    Sensor.enableSensorEvents(method(:onSensor));

    sensorListenerCallBackMethod = method(:onHeartRateData);

    var options = {
      :period => 1,
      :accelerometer => {
        :enabled => true,
        :sampleRate => 25,
      },
      :heartBeatIntervals => {
        :enabled => true,
      },
    };

    Sensor.registerSensorDataListener(sensorListenerCallBackMethod, options);
  }

  // Update the view
  function onUpdate(dc as Dc) as Void {
    System.println("Updating screen...");
    onLayout(dc);
  }

  function onHeartRateData(sensorData as Sensor.SensorData) as Void {
    if (sensorData has :heartRateData) {
      var hrData = sensorData.heartRateData;

      if (hrData.heartBeatIntervals.size() > 0) {
        ibiData = hrData.heartBeatIntervals;
      }

      WatchUi.requestUpdate();
    }

    // if (sensorData has :accel && sensorData.accel != null) {
    //   accel = sensorData.accel;
    //   System.println("AccelormeterData");
    // }
  }

  function onSensor(info as Sensor.Info) as Void {
    heartRate = null;
    pulseOx = null;

    if (info.heartRate != null) {
      heartRate = info.heartRate;
    }

    if (info.oxygenSaturation != null) {
      pulseOx = info.oxygenSaturation;
    }

    if (info has :accel && info.accel != null) {
      accel = info.accel;
      System.println("AccelormeterData");
    }

    updateMetrics();
    sendDataToServer();
    WatchUi.requestUpdate();
  }

  function updateMetrics() as Void {
    var activityInfo = ActivityMonitor.getInfo();

    if (activityInfo.stressScore != null) {
      stressScore = activityInfo.stressScore;
    }

    if (activityInfo.respirationRate != null) {
      respirationRate = activityInfo.respirationRate;
    }

    if (Toybox has :SensorHistory && SensorHistory has :getBodyBatteryHistory) {
      var bbIterator = SensorHistory.getBodyBatteryHistory({ :period => 1 }); // Retrieve the most recent sample
      var sample = bbIterator.next();
      if (sample != null) {
        bodyBattery = sample.data;
      }
    }

    // var sensorInfo = Sensor.getInfo();
    // if (sensorInfo has :accel && sensorInfo.accel != null) {
    //   accel = sensorInfo.accel;
    //   System.println("AccelormeterData");
    // }
  }

  function sendDataToServer() as Void {
    var url = "https://urlToServer.at/stress_data";

    var ibiJsonArray = [];
    for (var i = 0; i < ibiData.size(); i++) {
      ibiJsonArray.add(ibiData[i].toFloat());
    }

    var timestamp = Time.now().value();

    var accels = [];
    for (var i = 0; i < accel.size(); i++) {
      accels.add((accel[i] * 9.81) / 1000); // conversion from mg in m/s^2
    }

    var params = {};

    params["timestamp"] = timestamp;

    if (heartRate != null) {
      params["heartRate"] = heartRate;
    }

    if (pulseOx != null) {
      params["pulseOx"] = pulseOx;
    }

    if (respirationRate != null) {
      params["respirationRate"] = respirationRate;
    }

    if (bodyBattery != null) {
      params["bodyBattery"] = bodyBattery;
    }

    if (stressScore != null) {
      params["stressScore"] = stressScore;
    }

    params["accelxyz"] = accels;
    params["IBI"] = ibiJsonArray;

    //   "timestamp" => timestamp,
    //   "heartRate" => heartRate,
    //   "pulseOx" => pulseOx,
    //   "respirationRate" => respirationRate,
    //   "accelxyz" => accels,
    //   "IBI" => ibiJsonArray,
    //   "bodyBattery" => bodyBattery,
    //   "stressScore" => stressScore,
    // };

    var options = {
      :method => Communications.HTTP_REQUEST_METHOD_POST,
      :headers => {
        "Content-Type" => Communications.REQUEST_CONTENT_TYPE_JSON,
      },
      :responseType => Communications.HTTP_RESPONSE_CONTENT_TYPE_JSON,
    };

    Communications.makeWebRequest(url, params, options, method(:onResponse));
  }

  function onHide() as Void {
    if (sensorListenerCallBackMethod != null) {
      Sensor.unregisterSensorDataListener();
      sensorListenerCallBackMethod = null;
    }
  }

  function onResponse(responseCode as Number, responseData as String?) as Void {
    // respondData = responseCode.toString();
    // System.println("API Response Code: " + responseCode);
    // if (responseCode == -400) {
    //   System.println("🚨 ERROR: API response format is invalid!");
    //   respondCode = "🚨 Invalid API Response!" + responseCode;
    // } else if (responseCode == 200 || responseCode == 204) {
    //   System.println("✅ Data successfully sent!");
    //   respondCode = "✅ Data Sent!" + responseCode;
    // }
    // if (responseData != null) {
    //   System.println("Raw Response Data: " + responseData);
    //   respondData = responseData; // Zeige API-Antwort auf der Uhr an
    // } else {
    //   System.println("🚨 ERROR: No response data received.");
    //   respondData = "🚨 No Data Received!";
    // }
    // WatchUi.requestUpdate();
  }
}
