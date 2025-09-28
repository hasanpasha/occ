int putchar(int c);

int print_number(int n)
{
    if (n == 0)
        return 0;
    print_number(n / 10);   // print higher digits first
    putchar((n % 10) + 48); // then print the last digit
}

// int space = 32;

int print_helloworld(int index)
{
    int index_char = index + 48;

    putchar(72);
    putchar(101);
    putchar(108);
    putchar(108);
    putchar(111);
    putchar(44);
    putchar(32);
    putchar(87);
    putchar(111);
    putchar(114);
    putchar(108);
    putchar(100);
    putchar(33);

    putchar(32);
    if (index == 0)
        putchar(48);
    else
        print_number(index);
    putchar(32);

    putchar(10);
}

int main(void)
{
    int step = 1;
    for (int i = 0; i <= 10000000; i += step)
    {
        step = (i / 1000) + 1;
        print_helloworld(i);
    }
}