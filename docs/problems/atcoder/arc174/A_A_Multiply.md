# Problem: A_A_Multiply.pas

```pascal
program A_A_Multiply;
uses
    math;
const
    maxn = 300 * 1000;
var
    n, i, c: int32;
    current_mn, current_mx, mn, mx, ans: int64;
    a: array [1 .. maxn] of int32;
    s: array [0 .. maxn] of int64;

begin
    readln(n, c);

    s[0] := 0;
    mx := 0;
    mn := 0;
    current_mx := 0;
    current_mn := 0;

    for i := 1 to n do begin

        read(a[i]);
        s[i] := s[i-1] + a[i];

        current_mx := max(current_mx + a[i], a[i]);
        mx := max(mx, current_mx);

        current_mn := min(current_mn + a[i], a[i]);
        mn := min(mn, current_mn);

    end;
    readln;

    ans := s[n];
    if c > 0 then
        inc(ans, mx * (c-1))
    else
        inc(ans, mn * (c-1));

    writeln(ans);
end.

```
