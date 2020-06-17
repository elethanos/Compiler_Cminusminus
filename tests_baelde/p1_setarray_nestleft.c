int main() {
  int* t;
  t=malloc(8);
  t[t[0]=0]=7;
  printf("%d",t[0]);
  return 0;
}
