#include <stdio.h>
#include <stdlib.h>

int main() {
  char input[256];
  printf("Entrez une phrase contenant un nombre : ");
  fgets(input, sizeof(input), stdin);

  int number;
  if (sscanf(input, "%*[^0-9]%d", &number) == 1) {
    printf("Le nombre est : %d\n", number);
  } else {
    printf("Pas de nombre trouvé.\n");
  }

  return 0;
}
