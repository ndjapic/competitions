# Problem: B_Down_with_Brackets.pas

```pascal
program B_Down_with_Brackets;
{$MODE DELPHI}
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, h: int32;
    s: string;
    ans: boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(s);
        n := length(s);

        h := 0;
        ans := false;
        for i := 1 to n-1 do begin
            case s[i] of
                '(': inc(h);
                ')': dec(h);
            end;
            if not ans then ans := h = 0;
        end;

        if ans then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
