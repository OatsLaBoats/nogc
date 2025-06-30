#include "builtins.h"


static const ng_Int anInt=10;
static const struct ng_String aString=(struct ng_String){.cap=0,.len=11,.mem="Hello World"};

static ng_Unit ng_main(ng_Unit);

static ng_Unit ng_main(ng_Unit){
}


int main(void){ng_main();return 0;}