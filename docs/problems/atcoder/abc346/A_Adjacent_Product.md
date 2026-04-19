# Problem: A_Adjacent_Product.pas

```pascal
program A_Adjacent_Product;
const
    maxn = 100;
var
    n, i: int8;
    a: array [1 .. maxn] of int16;

begin
    readln(n);

    read(a[1]);
    for i := 2 to n do begin
        read(a[i]);
        write(a[i-1] * a[i]);
        if i < n then write(' ');
    end;
    readln;
    writeln;
end.

```
