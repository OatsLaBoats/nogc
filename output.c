#include "builtins.h"

static const ng_Int anInt=10;
static const struct ng_String aString=(struct ng_String){.cap=0,.len=26,.mem="Hello, Great Queen Lyra!\n"};

static ng_Int fibonacci(ng_Int i);
static ng_Int testFunc(ng_Bool x);
static ng_Int testMath(ng_Int a,ng_Int b);
static ng_Unit ng_main(ng_Unit);

static ng_Int fibonacci(ng_Int i){
ng_Int _ng_tmpvar_0;
if(ng_eqInt(i,0)){
_ng_tmpvar_0=0;
}
else {
ng_Int _ng_tmpvar_1;
if(ng_eqInt(i,1)){
_ng_tmpvar_1=1;
}
else {
_ng_tmpvar_1=ng_addInt(fibonacci(ng_subInt(i,1)),fibonacci(ng_subInt(i,2)));
}
_ng_tmpvar_0=_ng_tmpvar_1;
}
return _ng_tmpvar_0;
}

static ng_Int testFunc(ng_Bool x){
ng_Int _ng_tmpvar_0;
if(x){
_ng_tmpvar_0=10;
}
else {
_ng_tmpvar_0=20;
}
return _ng_tmpvar_0;
}

static ng_Int testMath(ng_Int a,ng_Int b){
return ng_addInt(a,ng_subInt(10,b));
}

static ng_Unit ng_main(ng_Unit){
ng_printLn(ng_sliceString(aString));
struct ng_String ls=(struct ng_String){.cap=0,.len=22,.mem="You look lovely today!"};
ng_Int _ng_tmpvar_0;
if(true){
_ng_tmpvar_0=10;
}
else {
_ng_tmpvar_0=20;
}
ng_printLn((struct ng_StringSlice){.len=15,.mem="You look great!"});
ng_dropString(ls);
}


int main(void){ng_main();return 0;}