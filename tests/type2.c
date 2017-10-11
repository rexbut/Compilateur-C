int test(int a, int b, int c, int d) {
	return a+b+c+d;
}
extern int putchar(int c);
extern int puts(char* s);
extern int printf();
extern void* malloc(int t);
extern void free(void * bloc);

int * f(int x) {
	printf("%p\n", &x);
	return &x;
}

void print_string(char *s) {
        char c;
        while (c = *s++) putchar(c);
}

int main() {
	int xt;
	int * yt;
	int x;
	int y;
	int cpt;
	int i;
	int* tab;
	int* px;
	int* py;
	double d;
	
	int maxl;
	int minl;
	unsigned long unl;
	
	tab = malloc(sizeof(int)*10);
	px = malloc(sizeof(int));
	py = malloc(sizeof(int));
	i = 5;
	cpt = 0;
	x = test(1,2,3,4);
	
	printf("%p\n", &xt);
	yt = f(xt);
	printf("%p\n",yt);
	
	/*putchar(65);
	putchar(10);
	puts("ca marche");
	
	printf("test1 : %d\n", (16 && 1));
	printf("test2 : %d\n", (551515661 && 0));
	printf("test3 : %d\n", (0 && 1));
	printf("test4 : %d\n", (0 && 0));
	printf("test5 : %d\n", (1 || 1));
	printf("test6 : %d\n", (1 || 0));
	printf("test7 : %d\n", (0 || 1));
	printf("test8 : %d\n", (0 || 0));
	
	printf("test9 : %d\n", cpt);
	printf("test10 : %d\n", cpt++);
	printf("test11 : %d\n", cpt);
	printf("test12 : %d\n", ++cpt);
	printf("test13 : %d\n", cpt);
	printf("test14 : %d\n", cpt--);
	printf("test15 : %d\n", cpt);
	printf("test16 : %d\n", --cpt);
	printf("test17 : %d\n", cpt);
	
	printf("\n");
	
	printf("test18 : %d\n", !1);
	printf("test19 : %d\n", !0);
	
	printf("\n");
	
	printf("test20 : %d\n", -5);
	printf("test21 : %d\n", -1000);
	
	printf("\n");
	
	printf("test22 : %d\n", +5);
	printf("test23 : %d\n", +1000);
	
	printf("\n");
	
	printf("test22 : %d\n", -6 + 5);
	printf("test23 : %d\n", -1000 + 5000);
	
	printf("\n");
	
	printf("test24 : %d\n", i);
	printf("test25 : %d\n", &i);
	
	printf("test26 : %d\n", 1);
	
	printf("\n");
	
	printf("test27 : %d\n", 10/2);
	printf("test28 : %d\n", 11%2);
	printf("test29 : %d\n", -10/2);
	printf("test30 : %d\n", 10/-2);
	printf("test31 : %d\n", -10/-2);
	
	printf("\n");
	
	printf("int : %d\n", sizeof(int));
	printf("char : %d\n", sizeof(char));
	printf("short : %d\n", sizeof(short));
	printf("long : %d\n", sizeof(long));
	printf("double : %d\n", sizeof(double));
	
	printf("unsigned int : %d\n", sizeof(unsigned int));
	printf("unsigned char : %d\n", sizeof(unsigned char));
	printf("unsigned short : %d\n", sizeof(unsigned short));
	printf("unsigned long : %d\n", sizeof(unsigned long));
	
	for(y=0; y <10; y++) {
		tab[y] = y;
	}
	
	for(y=0; y <10; y++) {
		printf("tab[%d] : %d\n", y, tab[y]);
	}
	
	while(0){
		if(cpt && cpt){
			printf("%d\n", cpt);
		}
		
		cpt = cpt+1;
	}
	printf("test31 : %d\n", tab);
	printf("test32 : %d\n", (tab+2));
	printf("test33 : %d\n", (2+tab));
	printf("test34 : %d\n", (tab+16)-tab);
	
	minl = -9223372036854775806L;
	maxl = 	9223372036854775806L;
	unl = 18446744073709551614L;
	printf("minl : %d\n", minl);
	printf("maxl : %d\n", maxl);
	printf("unl : %lu\n", unl+1);*/
	d = 10.1;
	printf("test35 : %f\n", d);
	
	return x;
}

