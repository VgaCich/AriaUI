#ifndef EXTERNAL_TRANSPORT_H
#define EXTERNAL_TRANSPORT_H

#include <Windows.h>

typedef struct ExternalTransportResponse {
    void *Data;
    int Length;
    void __stdcall (*Free)(struct ExternalTransportResponse *self);
} ExternalTransportResponse;

typedef struct ExternalTransportInstance {
    BOOL __stdcall (*Connect)(struct ExternalTransportInstance *self, LPCSTR server, WORD port, LPCSTR user, LPCSTR password, BOOL ssl);
    void __stdcall (*Disconnect)(struct ExternalTransportInstance *self);
    ExternalTransportResponse* __stdcall (*SendRequest)(struct ExternalTransportInstance *self, void *Data, int Length);
    void __stdcall (*Free)(struct ExternalTransportInstance *self);
} ExternalTransportInstance;

#endif