# Задатак: A_Dr_TC.pas

```pascal
program A_Dr_TC;
{$MODE DELPHI}{$INLINE ON}
var
    ntc, tci: int16;
    n, i, ans: int8;
    s: string;

begin
    randomize;
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        ans := 0;
        for i := 1 to n do
            if s[i] = '1' then inc(ans);

        writeln(ans * (n-2) + n);

    end;
end.

```
