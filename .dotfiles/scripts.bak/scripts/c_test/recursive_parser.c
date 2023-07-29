#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main() {
    char input[256];
    printf("Entrez une phrase contenant un nombre : ");
    fgets(input, sizeof(input), stdin);

    int number;
    char *p = input;

    while (*p) {
        if (isdigit(*p)) {
            if (sscanf(p, "%d", &number) == 1) {
                printf("%d\n", number);
                char buffer[20];
                sprintf(buffer, "%d", number);
                p += strlen(buffer);
            }
        }
        p++;
    }

    return 0;
}
