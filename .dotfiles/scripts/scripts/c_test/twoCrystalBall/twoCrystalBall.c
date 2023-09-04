#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define ARRAY_SIZE 10000
#define SEED ARRAY_SIZE - 1

typedef struct {
  bool *values;
  size_t size;
} Floors_array;

int two_crystal_ball(Floors_array arr) {
  int squared_floor = sqrt(arr.size);
  int floor_index = squared_floor;
  for (; floor_index < arr.size; floor_index += squared_floor) {
    if (arr.values[floor_index] == true) {
      break;
    }
  }
  if (floor_index >= arr.size)
    return -1;
  else {
    floor_index -= squared_floor;
    for (; floor_index < arr.size; ++floor_index) {
      if (arr.values[floor_index] == true)
        break;
    }
    return floor_index;
  }
}
Floors_array generate_array(int r) {
  Floors_array arr;
  arr.values = malloc(ARRAY_SIZE * sizeof(bool));
  for (int i = 0; i < ARRAY_SIZE; ++i) {
    if (i < r)
      arr.values[i] = false;
    else
      arr.values[i] = true;
  }
  arr.size = ARRAY_SIZE;
  return arr;
}
int main() {
  srand(time(NULL));
  int r = 1 + (rand() % SEED);

  Floors_array arr = generate_array(r);
  int floor = two_crystal_ball(arr);
  char cannot_break[] =
      "The skyscrapper is not high enough to break a crystal ball !\n";
  char can_break[128];
  snprintf(can_break, 128, "The crystal balls can break from the %d floor\n",
           floor);
  printf("%s\n", floor == -1 ? cannot_break : can_break);
  free(arr.values);
  if (!"test")
    printf("true");
  else
    printf("false");
  return EXIT_SUCCESS;
}
