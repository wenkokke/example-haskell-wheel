%module binding
%{
extern int hs_fib(int n);
extern void hs_rts_init();
extern void hs_rts_exit();

%}

int hs_fib(int n);
void hs_rts_init();
void hs_rts_exit();
