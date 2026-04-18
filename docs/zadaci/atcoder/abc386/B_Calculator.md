# Задатак: B_Calculator.pas

```pascal
program B_Calculator;
{$mode delphi}
const
    nn = 1000;
var
    n, i, ans: int16;
    s: string;

begin
    readln(s);
    n := length(s);

    ans := 0;
    i := 1;
    while i <= n do begin
        if (i < n) and (s[i] = '0') and (s[i+1] = '0') then inc(i);
        inc(ans);
        inc(i);
    end;

    writeln(ans);
end.

```
