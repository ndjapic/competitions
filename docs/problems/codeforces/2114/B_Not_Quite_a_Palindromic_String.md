# Problem: B_Not_Quite_a_Palindromic_String.pas

```pascal
program B_Not_Quite_a_Palindromic_String;
{$MODE DELPHI}
uses
    math;
var
    ntc, tci: int16;
    n, k, i, c1, c0: int32;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);
        readln(s);

        c1 := 0;
        for i := 1 to n do
            if s[i] = '1' then inc(c1);

        c0 := n-c1;
        k := n div 2 - k;

        if (min(c0, c1) >= k) and not odd(c0-k) and not odd(c1-k) then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
