#include <stdio.h>

void qsort(int v[], int left, int right)
{
  int i, last, tmp;
  if (left >= right)
    return;
  tmp = v[left];
  v[left] = v[(left+right)/2];
  v[(left+right)/2] = tmp;
  last = left;
  for (i = left+1; i <= right; i++) {
    if (v[i] < v[left]) {
      ++last;
      tmp = v[last];
      v[last] = v[i];
      v[i] = tmp;
    }
  }
  tmp = v[left];
  v[left] = v[last];
  v[last] = tmp;
  qsort(v, left, last-1);
  qsort(v, last+1, right);
}

void main()
{
  int vector[10] = { 56, 54, 32, 78, 59, 16, 32, 1, 77, -17 };
  int i;
  qsort(vector, 0, 9);
  for (i=0; i<10; i++) printf("%d\n", vector[i]);
}
