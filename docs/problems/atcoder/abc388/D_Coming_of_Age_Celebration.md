# Problem: D_Coming_of_Age_Celebration.pas

```pascal
program D_Coming_of_Age_Celebration;
uses
    math;
const
    nn = 500 * 1000 + 1;

var
    n, i, mn: int32;
    a, d: array [1 .. nn] of int32;

begin
    readln(n);

    for i := 1 to n+1 do d[i] := 0;

    for i := 1 to n do begin
        read(a[i]);
        inc(a[i], d[i]);
        mn := min(a[i], n-i);
        dec(a[i], mn);
        write(a[i], ' ');
        inc(d[i+1]);
        dec(d[i+1+mn]);
        inc(d[i+1], d[i]);
    end;
    readln;
    writeln;
end.

```
