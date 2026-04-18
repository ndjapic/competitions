# Задатак: M_Triangle_Construction.pas

```pascal
program M_Triangle_Construction;
uses
    math;
const
    maxn = 200 * 1000;
var
    n, i, m: int32;
    s, ans: int64;
    a: array [1 .. maxn] of int32;

begin
    readln(n);
    s := 0;
    m := 0;

    for i := 1 to n do begin
        read(a[i]);
        inc(s, a[i]);
        m := max(m, a[i]);
    end;
    readln;

    ans := min(s div 3, s-m);
    writeln(ans);
end.

```
