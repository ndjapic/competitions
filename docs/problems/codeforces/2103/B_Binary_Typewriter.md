# Problem: B_Binary_Typewriter.pas

```pascal
program B_Binary_Typewriter;
{$MODE DELPHI}
uses
    math;
var
    ntc, tci: int16;
    n, i, c01, c10, ans: int32;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        ans := n;
        for i := 2 to n do
            if s[i] <> s[i-1] then inc(ans);
        if s[1] = '1' then inc(ans);

        c01 := 0;
        c10 := 0;
        if s[1] = '1' then inc(c01);

        for i := 2 to n do
            if s[i-1] <> s[i] then
                case s[i] of
                    '0': inc(c10);
                    '1': inc(c01);
                end;

        if max(c01, c10) >= 2 then
            dec(ans, 2)
        else if min(c01, c10) >= 1 then
            dec(ans);

        writeln(ans);

    end;
end.

```
