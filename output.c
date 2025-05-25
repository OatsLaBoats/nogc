#include "builtins.h"

static const ng_Int anInt=10;
static const struct ng_String aString=(struct ng_String){.cap=0,.len=26,.mem="Hello, Great Queen Lyra!\n"};

static ng_Int testMath(ng_Int a,ng_Int b);
static ng_Unit ng_main(ng_Unit);

static ng_Int testMath(ng_Int a,ng_Int b){
ng_Int _ng_tmpvar_0;
_ng_tmpvar_0=10;
ng_Int _ng_tmpvar_1;
_ng_tmpvar_1=ng_subInt(_ng_tmpvar_0,b);
ng_Int _ng_tmpvar_2;
_ng_tmpvar_2=ng_addInt(a,_ng_tmpvar_1);
return _ng_tmpvar_2;
}

static ng_Unit ng_main(ng_Unit){
struct ng_StringSlice _ng_tmpvar_0;
_ng_tmpvar_0=ng_sliceString(aString);
ng_printLn(_ng_tmpvar_0);
struct ng_String ls;
struct ng_String _ng_tmpvar_1;
_ng_tmpvar_1=(struct ng_String){.cap=0,.len=22,.mem="You look lovely today!"};
ls=_ng_tmpvar_1;
ng_Int _ng_tmpvar_2;
_ng_tmpvar_2=10;
ng_Int _ng_tmpvar_3;
_ng_tmpvar_3=4;
ng_Int _ng_tmpvar_4;
_ng_tmpvar_4=testMath(_ng_tmpvar_2,_ng_tmpvar_3);
struct ng_StringSlice _ng_tmpvar_5;
_ng_tmpvar_5=(struct ng_StringSlice){.len=15,.mem="You look great!"};
ng_printLn(_ng_tmpvar_5);
ng_dropString(ls);
}


int main(void){ng_main();return 0;}