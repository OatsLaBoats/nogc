#include "builtins.h"


static const ng_Int anInt=10;
static const struct ng_String aString=(struct ng_String){.cap=0,.len=11,.mem="Hello World"};

static ng_Int myAdd(ng_Int x,ng_Int y);
static ng_Int myAdd2(ng_Int x,ng_Int y);
static ng_Unit ng_main(ng_Unit);

static ng_Int myAdd(ng_Int x,ng_Int y){
return ng_addInt(x,y);
}

static ng_Int myAdd2(ng_Int x,ng_Int y){
return myAdd(x,y);
}

static ng_Unit ng_main(ng_Unit){
}


int main(void){ng_main();return 0;}