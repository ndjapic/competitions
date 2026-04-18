# Задатак: E_I_Hate_Sigma_Problems.pas

```pascal
program E_I_Hate_Sigma_Problems;
uses
    math;
const
    nn = 200 * 1000;
var
    n, i, x: int32;
    s, ans: int64;
    last: array [1 .. nn] of int32;

begin
    readln(n);
    for x := 1 to n do last[x] := 0;

    ans := 0;
    s := 0;
    for i := 1 to n do begin
        read(x);
        inc(s, i - last[x]);
        inc(ans, s);
        last[x] := i;
    end;
    readln;

    writeln(ans);
end.

```
