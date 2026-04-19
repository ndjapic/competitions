# Problem: A_Serval_and_String_Theory.pas

```pascal
program A_Serval_and_String_Theory;
{$MODE DELPHI}
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int32;
    n, k, l, r, i: int32;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);
        readln(s);

        if k = 0 then begin

            l := 1;
            r := n;
            while (l < r) and (s[l] = s[r]) do begin
                inc(l);
                dec(r);
            end;

            if l >= r then
                writeln('NO')
            else if s[l] < s[r] then
                writeln('YES')
            else
                writeln('NO');

        end else begin

            i := 2;
            while (i <= n) and (s[i] = s[1]) do inc(i);

            if i <= n then
                writeln('YES')
            else
                writeln('NO');

        end;

    end;
end.

```
