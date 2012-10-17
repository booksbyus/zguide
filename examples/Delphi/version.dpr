program version;
//
//  Report 0MQ version
//

{$APPTYPE CONSOLE}

uses
    SysUtils
  , zmqapi
  ;

var
  major,
  minor,
  patch: Integer;
begin
  ZMQVersion( major, minor, patch );
  Writeln( Format( 'Current 0MQ version is %d.%d.%d', [major, minor, patch]) );
end.
