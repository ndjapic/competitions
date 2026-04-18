# Задатак: B_Expensive_Number.pas

```pascal
program B_Expensive_Number;
{$MODE DELPHI}
var
    ntc, tci: int16;
    s: string;
    n, i, c: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(s);
        n := length(s);

        while s[n] = '0' do dec(n);

        c := length(s) - n;
        for i := 1 to n-1 do
            if s[i] <> '0' then inc(c);

        writeln(c);

    end;

end.

```
