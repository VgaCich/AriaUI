type
  PExternalTransportResponse = ^TExternalTransportResponse;
  TExternalTransportResponse = record
    Data: Pointer;
    Length: Integer;
    Free: procedure(Self: PExternalTransportResponse); stdcall;
  end;
  PExternalTransportInstance = ^TExternalTransportInstance;
  TExternalTransportInstance = record
    Connect: function(Self: PExternalTransportInstance; Server: PChar; Port: Word; UserName, Password: PChar; UseSSL: LongBool): LongBool; stdcall;
    Disconnect: procedure(Self: PExternalTransportInstance); stdcall;
    SendRequest: function(Self: PExternalTransportInstance; Data: Pointer; Length: Integer): PExternalTransportResponse; stdcall;
    Free: procedure(Self: PExternalTransportInstance); stdcall;
  end;
  TExternalTransportCreate = function: PExternalTransportInstance; stdcall;