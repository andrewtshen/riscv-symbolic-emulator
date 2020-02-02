int sum(int a, int b);

int main() {
    int a = 3; int b = 5; 
    sum(3, 5);

    asm volatile ("uret");
}

int sum(int a, int b) {
    return a + b;
}