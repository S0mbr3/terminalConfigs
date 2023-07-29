#include <stdio.h>
#include <stdlib.h>

int main() {
  char* input = NULL;
  size_t input_size = 0;
  printf("Enter a string: ");
  getline(&input, &input_size, stdin); // getline alloue automatiquement la mémoire nécessaire
  printf("You entered: %s", input);
  free(input); // libérer la mémoire allouée par getline
  return 0;
}
