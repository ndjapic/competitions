# Задатак: C_One_Time_Swap.pas

```pascal
program C_One_Time_Swap;
{$H+}
uses
    math;
var
    n, i, mx: int32;
    ans: int64;
    s: string;
    ch: char;
    c: array ['a' .. 'z'] of int32;

function nc2(n: int32): int64;
begin
    nc2 := int64(n-1) * n div 2;
end;

begin
    readln(s);
    n := length(s);

    for ch := 'a' to 'z' do c[ch] := 0;

    for i := 1 to n do inc(c[s[i]]);

    ans := nc2(n);
    mx := 1;

    for ch := 'a' to 'z' do begin
        dec(ans, nc2(c[ch]));
        mx := max(mx, c[ch]);
    end;

    if mx > 1 then inc(ans);
    writeln(ans);
end.

```
