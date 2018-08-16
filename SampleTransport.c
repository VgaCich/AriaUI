#include <stdlib.h>
#include <stdio.h>
#include "ExternalTransport.h"
#include <WinInet.h>

#ifdef __TINYC__
#define EXPORT __stdcall __attribute__((dllexport, nodecorate))
#else
#define EXPORT __stdcall __attribute__((dllexport))
#endif

typedef struct TransportInstance {
    ExternalTransportInstance base;
    HINTERNET session, conn;
    DWORD flags;
    CRITICAL_SECTION reqlock;
} TransportInstance;

void __stdcall FreeResponse(struct ExternalTransportResponse *self)
{
    if(!self) return;
    free(self->Data);
    free(self);
}

BOOL __stdcall Connect(struct ExternalTransportInstance *self, LPCSTR server, WORD port, LPCSTR user, LPCSTR password, BOOL ssl)
{
    TransportInstance *inst = self;
    if(!self || !inst->session) return FALSE;
    self->Disconnect(self);
    EnterCriticalSection(&inst->reqlock);
    inst->conn = InternetConnect(inst->session, server, port, user, password, INTERNET_SERVICE_HTTP, INTERNET_FLAG_EXISTING_CONNECT, 0);
    if(!inst->conn)
    {
        LeaveCriticalSection(&inst->reqlock);
        return FALSE;
    }
    inst->flags = INTERNET_FLAG_KEEP_CONNECTION | INTERNET_FLAG_DONT_CACHE | INTERNET_FLAG_NO_COOKIES | INTERNET_FLAG_PRAGMA_NOCACHE | INTERNET_FLAG_RELOAD | (ssl ? INTERNET_FLAG_SECURE : 0);
    LeaveCriticalSection(&inst->reqlock);
    return TRUE;
}

void __stdcall Disconnect(struct ExternalTransportInstance *self)
{
    if(!self) return;
    TransportInstance *inst = self;
    EnterCriticalSection(&inst->reqlock);
    if(inst->conn)
        InternetCloseHandle(inst->conn);
    inst->conn = NULL;
    LeaveCriticalSection(&inst->reqlock);
}

ExternalTransportResponse* __stdcall SendRequest(struct ExternalTransportInstance *self, void *Data, int Length)
{
    TransportInstance *inst = self;
    if(!self || !inst->conn) return NULL;
    EnterCriticalSection(&inst->reqlock);
    HINTERNET req = HttpOpenRequest(inst->conn, "POST", "/jsonrpc", NULL, NULL, NULL, inst->flags, 0);
    if(!req) goto error;
    char headers[30];
    sprintf(headers, "Content-Length: %d", Length);
    if(!HttpSendRequest(req, headers, -1L, Data, Length)) goto error;
    ExternalTransportResponse *res = calloc(1, sizeof(ExternalTransportResponse));
    res->Free = &FreeResponse;
    DWORD avail, read;
    while(InternetQueryDataAvailable(req, &avail, 0, 0))
    {
        if(!avail) break;
        res->Data = realloc(res->Data, res->Length + avail);
        if(!InternetReadFile(req, res->Data + res->Length, avail, &read)) break;
        res->Length += read;
    }
    InternetCloseHandle(req);
    LeaveCriticalSection(&inst->reqlock);
    return res;
error:
    if(req) InternetCloseHandle(req);
    LeaveCriticalSection(&inst->reqlock);
    return NULL;
}

void __stdcall FreeInstance(struct ExternalTransportInstance *self)
{
    if(!self) return;
    TransportInstance *inst = self;
    self->Disconnect(self);
    if(inst->session)
        InternetCloseHandle(inst->session);
    DeleteCriticalSection(&inst->reqlock);
    free(self);
}

void* EXPORT TransportCreate(void)
{
    TransportInstance *inst = calloc(1, sizeof(TransportInstance));
    inst->base.Connect = &Connect;
    inst->base.Disconnect = &Disconnect;
    inst->base.SendRequest = &SendRequest;
    inst->base.Free = &FreeInstance;
    InitializeCriticalSection(&inst->reqlock);
    inst->session = InternetOpen("AriaUI Sample Transport", INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, 0);
    return inst;
}

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
    return TRUE;
}