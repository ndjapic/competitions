# Problem: B_Prefix_and_Suffix.pas

```pascal
program B_Prefix_and_Suffix;
uses
    math;
const
    maxn = 100;
var
    n, m, i, j, ans: int8;
    s, t: array [1 .. maxn] of char;

begin
    readln(n, m);
    for i := 1 to n do read(s[i]); readln;
    for j := 1 to m do read(t[j]); readln;

    ans := 0;

    i := 1;
    j := 1;
    while (i <= n) and (s[i] = t[j]) do begin
        inc(i);
        inc(j);
    end;

    if i <= n then inc(ans, 2);

    i := n;
    j := m;
    while (i > 0) and (s[i] = t[j]) do begin
        dec(i);
        dec(j);
    end;

    if i > 0 then inc(ans);

    writeln(ans);
end.


```
