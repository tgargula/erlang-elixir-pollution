-author("tgargula").

-record(measurement, {time, type, value}).

-define(STATION_BUCKET, <<"StationBucket">>).
-define(MEASUREMENT_BUCKET, <<"MeasurementBucket">>).
