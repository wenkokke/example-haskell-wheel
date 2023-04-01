%module binding
%{
#include "Fib_stub.h"

int hs_fib(int n);

void hs_rts_init()
{
  hs_init(0, 0);
}

void hs_rts_exit()
{
  void hs_exit();
}

%}

int hs_fib(int n);
void hs_rts_init();
void hs_rts_exit();
