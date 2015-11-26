int emitters[] = {2, 3, 4, 5, 6, 7, 8};
int n_emitters = 7;
int emitter_i = 0;

int sensors[] = {A0, A1, A2, A3, A4, A5};
int n_sensors = 6;
int sensor_i = 0;
int sensor_value = 0;

unsigned long time;

void off(int emitter_i) {
  digitalWrite(emitters[emitter_i], HIGH);
}
void on(int emitter_i) {
  digitalWrite(emitters[emitter_i], LOW);
}
int sample(int sensor_i) {
  return analogRead(sensors[sensor_i]);
}

void setup() {
  Serial.begin(115200);
  for(emitter_i=0; emitter_i<n_emitters; emitter_i++) {
    pinMode(emitters[emitter_i], OUTPUT);
    off(emitter_i);
  }
  for(sensor_i=0; sensor_i<n_sensors; sensor_i++) {
    pinMode(sensors[sensor_i], INPUT);
  }
}

void loop() {
  for(sensor_i=0; sensor_i<n_sensors; sensor_i++) {
    for(emitter_i=0; emitter_i<n_emitters; emitter_i++) {
      time = millis();
      on(emitter_i);
      sensor_value = sample(sensor_i);
      off(emitter_i);
      Serial.print(
        time + String(",") + sensor_i + String(",") + emitter_i + String(",") + sensor_value + String("\n")
      );
    }
  }
  //Serial.print("0,0,0,0\n");
  //delay(30);
}
