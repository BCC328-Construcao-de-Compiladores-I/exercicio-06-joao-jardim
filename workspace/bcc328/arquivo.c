#include <stdio.h>
// code generated for L2 expressions
int main () {
    int x = 0;
    {
        int x = 1;
        int a = (x + 1);
        printf("%d\n", a);
        {
            int x = 5;
            int a = (x - 1);
            printf("%d\n", a);
        }
        printf("%d\n", x);
    }
    printf("Digite o valor de x:\n");
    scanf("%d", &x);
    printf("%d\n", (0 + (x * x)));
    return 0;
}
