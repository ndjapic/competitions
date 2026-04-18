# Задатак: C_Count_Arithmetic_Subarrays.pas

```pascal
program C_Count_Arithmetic_Subarrays;
const
    nn = 200 * 1000;
var
    n, l, r: int32;
    ans: int64;
    a: array [1 .. nn] of int32;

begin
    readln(n);
    ans := n;
    read(a[1]);
    l := 1;

    for r := 2 to n do begin
        read(a[r]);
        while a[l+1] - a[l] <> a[r] - a[r-1] do inc(l);
        inc(ans, r-l);
    end;

    writeln(ans);
end.

```
