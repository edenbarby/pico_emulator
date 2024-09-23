int fib(int it) {
  int a = 1;
  int b = 1;
  while (it--) {
    int c = a + b;
    a = b;
    b = c;
  }
  return b;
}

void end() {
  while (1) {
  }
}

int main(void) {
  int a = 7;
  int b = fib(a);
  // end();
  return b;
}
