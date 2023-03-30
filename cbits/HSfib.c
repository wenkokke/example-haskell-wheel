#include "Fib_stub.h"

#if defined(_WIN32)
#define DllExport __declspec(dllexport)
#else
#define DllExport
#endif

DllExport
int hs_fib(int n);

DllExport
void hs_rts_init()
{
  hs_init(0, 0);
}

DllExport
void hs_rts_exit()
{
  void hs_exit();
}
