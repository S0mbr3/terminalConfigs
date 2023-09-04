#include "stdio.h"
#include "stdbool.h"

#define arrayLength(array) (sizeof(array) / sizeof(array[0]))

typedef struct {
  int *values;  
  size_t size;
} intArray;

bool calculate(int arr[], int max, int min, int needle) {
  int median = max - (min / 2);
  if (min > max || min < 0) {
    return false;
      }
  if (needle == arr[median]) {
    return true;
  }
  else if (needle > arr[median]){
    return calculate(arr, max, median+1, needle);
  }
  else if (needle < arr[median]){
    return calculate(arr, median-1, min, needle);
  }
  else return false;
}

bool bs(intArray arr, int needle) {
  return calculate(arr.values, arr.size-1 , 0, needle);
}

int main() {
  int ar[] = {1,12,34,42,58,61,72,84,93,101};
  intArray arr;
  arr.values = ar;
  arr.size = arrayLength(ar);
  int needle = 42;
  printf("The number %d %s\n", needle, bs(arr, needle) == true ? "is in the array" : "is not in the array");
  return 0;
}
