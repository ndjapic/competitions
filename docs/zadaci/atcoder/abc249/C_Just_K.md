# Задатак: C_Just_K.pas

```pascal
program C_Just_K;
{$H+}
uses
    math;
const
    nn = 15;
var
    n, k, i, j, x, ans: int8;
    mask: int32;
    ch: char;
    s: array [1 .. nn] of string;
    c: array ['a' .. 'z'] of int8;

begin
    readln(n, k);

    for i := 1 to n do readln(s[i]);

    ans := 0;
    for mask := 0 to (int32(1) shl n) - 1 do begin

        for ch := 'a' to 'z' do c[ch] := 0;

        for i := 1 to n do
            if odd(mask shr (i-1)) then
                for j := 1 to length(s[i]) do inc(c[s[i][j]]);

        x := 0;
        for ch := 'a' to 'z' do
            if c[ch] = k then inc(x);
        ans := max(ans, x);

    end;

    writeln(ans);
end.

```
