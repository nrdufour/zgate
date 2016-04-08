# ZGate

ZGate is an experiment to read and process the Zigbee API packets sent by a bunch of wireless sensors.
The application is:

+ reading the serial device where the zigbee coordinator is
+ decode the zigbee api packet
+ process the content to transform it to a document containing the current date and the sensor zigbee unique address
+ send the document to a Barrel-DB instance

It's still very early stage. It works with my setup and I need to make it configurable and if possible in a package like a release.
