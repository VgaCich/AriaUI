#include <stdlib.h>
#include <stdio.h>
#include <Windows.h>
#include <WinInet.h>

#ifdef __TINYC__
#define EXPORT __stdcall __attribute__((dllexport, nodecorate))
#else
#define EXPORT __stdcall __attribute__((dllexport))
#endif

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
    return TRUE;
}

typedef struct ExternalTransportResponce {
    void *data;
    int len;
    void __stdcall (*free)(struct ExternalTransportResponce *p);
} ExternalTransportResponce;

typedef struct TransportInstance {
    HINTERNET session, conn;
    DWORD flags;
    CRITICAL_SECTION reqlock;
} TransportInstance;

void __stdcall FreeResponce(struct ExternalTransportResponce *p)
{
    if(!p) return;
    free(p->data);
    free(p);
}

void* EXPORT Create(void)
{
    TransportInstance *inst = calloc(1, sizeof(TransportInstance));
    InitializeCriticalSection(&inst->reqlock);
    inst->session = InternetOpen("AriaUI Sample Transport", INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, 0);
    return inst;
}

void EXPORT Disconnect(TransportInstance *inst)
{
    if(!inst) return;
    EnterCriticalSection(&inst->reqlock);
    if(inst->conn)
        InternetCloseHandle(inst->conn);
    inst->conn = NULL;
    LeaveCriticalSection(&inst->reqlock);
}

void EXPORT Free(TransportInstance *inst)
{
    if(!inst) return;
    Disconnect(inst);
    if(inst->session)
        InternetCloseHandle(inst->session);
    DeleteCriticalSection(&inst->reqlock);
    free(inst);
}

BOOL EXPORT Connect(TransportInstance *inst, char *server, WORD port, char *user, char *password, BOOL ssl)
{
    if(!inst || !inst->session) return FALSE;
    Disconnect(inst);
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

ExternalTransportResponce* EXPORT SendRequest(TransportInstance *inst, void *data, int len)
{
    if(!inst || !inst->conn) return NULL;
    EnterCriticalSection(&inst->reqlock);
    HINTERNET req = HttpOpenRequest(inst->conn, "POST", "/jsonrpc", NULL, NULL, NULL, inst->flags, 0);
    if(!req) goto error;
    char headers[30];
    sprintf(headers, "Content-Length: %d", len);
    if(!HttpSendRequest(req, headers, -1L, data, len)) goto error;
    ExternalTransportResponce *res = calloc(1, sizeof(ExternalTransportResponce));
    res->free = &FreeResponce;
    DWORD avail, read;
    while(InternetQueryDataAvailable(req, &avail, 0, 0))
    {
        if(!avail) break;
        res->data = realloc(res->data, res->len + avail);
        if(!InternetReadFile(req, res->data + res->len, avail, &read)) break;
        res->len += read;
    }
    InternetCloseHandle(req);
    LeaveCriticalSection(&inst->reqlock);
    return res;
error:
    if(req) InternetCloseHandle(req);
    LeaveCriticalSection(&inst->reqlock);
    return NULL;
}
