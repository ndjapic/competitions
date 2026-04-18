# Задатак: A_Brogramming_Contest.pas

```pascal
program A_Brogramming_Contest;
{$MODE DELPHI}
var
	ntc, tci: int8;
    n, i, ans: int16;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);
		readln(s);

        ans := 0;
        for i := 1 to n do
            case s[i] of
                '0': if (i > 1) and (s[i-1] = '1') then inc(ans);
                '1': if (i = 1) or (s[i-1] = '0') then inc(ans);
            end;

        writeln(ans);

    end;
end.

```
