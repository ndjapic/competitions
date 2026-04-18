# Задатак: D_ABA.pas

```pascal
program D_ABA;
{$mode delphi}
uses
    math;
const
    nn = 200 * 1000;
var
    n, i: int32;
    ans: int64;
    s: string;
    ch: char;
    c: array[0 .. nn, 'A' .. 'Z'] of int64;

begin
    readln(s);
    n := length(s);

    for ch := 'A' to 'Z' do c[0, ch] := 0;

    for i := 1 to n do begin
        for ch := 'A' to 'Z' do c[i, ch] := c[i-1, ch];
        inc(c[i, s[i]]);
    end;

    ans := 0;
    for i := 2 to n-1 do
        for ch := 'A' to 'Z' do
            inc(ans, c[i-1, ch] * (c[n, ch] - c[i, ch]));
    writeln(ans);
end.

```
