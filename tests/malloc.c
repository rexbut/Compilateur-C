extern void *malloc(unsigned long p);
extern void free (void * p);
extern int printf();

int main() {

        int* copy;
		//printf("%d\n", 0);
		
		copy = malloc(1);
        printf("0: %d\n", copy);
		copy = malloc(1 * sizeof(int));
        printf("1: %d\n", copy[0]);
		free(copy);
		printf("1 free: %d\n", copy[0]);
		copy = malloc(2 * sizeof(int));
        printf("2: %d\n", copy);
		copy = malloc(3 * sizeof(int));
        printf("3: %d\n", copy);
		copy = malloc(4 * sizeof(int));
        printf("4: %d\n", copy);
		copy = malloc(5 * sizeof(int));
        printf("5: %d\n", copy);
		copy = malloc(6 * sizeof(int));
        printf("6: %d\n", copy);
        free(copy);

        return 0;

}
