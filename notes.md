# lowering

This C program

```c
long main() {
    long a = 0;
    int b = 0;
    return 0;
}
```

should lower into the following unoptimized IR

```llvmir
def main() {
  bb0:
    %a = alloca, size=8, align=8
    store %a, 0, size=8, align=8
    %b = alloca, size=4, align=4
    store %b, 0, size=4, align=4
    ret 0
}
```

this IR can then be lowered to very sane machine code ignoring stack alignment which also like matters

```x86asm
sub rbp, 8    ; a
mov [rbp], 0
sub rbp, 4    ; b
xor rax, rax
ret
```

---

```c
int main()
{
    int a = 1 + 3 * 4;
    return 0;
}
```

```llvmir
def main() {
  bb0:
    %a = alloca, size=4, align=4
    %1 = mul 3, 4
    %2 = add 1, %1
    store %a, %2, size=4, align=4
    ret 0
}
```
