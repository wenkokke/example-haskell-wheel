%module binding
%{
#if defined(_WIN32)
#define DllImport __declspec(dllimport)
#else
#define DllImport
#endif

DllImport
extern int hs_fib(int n);

DllImport
extern void hs_rts_init();

DllImport
extern void hs_rts_exit();
%}

int hs_fib(int n);
void hs_rts_init();
void hs_rts_exit();
