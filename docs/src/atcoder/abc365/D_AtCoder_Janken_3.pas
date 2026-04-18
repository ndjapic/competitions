program D_AtCoder_Janken_3;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 200 * 1000;
var
    n, i, ans0, ans2: int32;
    s: string;
    a, t: array [1 .. nn] of int8;

begin
    readln(n);
    readln(s);

    ans2 := n;
    for i := 1 to n do begin

        case s[i] of
            'P': a[i] := 0;
            'R': a[i] := 1;
            'S': a[i] := 2;
        end;

        t[i] := (a[i] + 2) mod 3;
        if (i > 1) and (t[i-1] = t[i]) then begin
            t[i] := a[i];
            dec(ans2);
        end;

    end;

    ans0 := n-1;
    t[1] := a[1];
    for i := 2 to n do begin

        t[i] := (a[i] + 2) mod 3;
        if t[i-1] = t[i] then begin
            t[i] := a[i];
            dec(ans0);
        end;

    end;

    writeln(max(ans0, ans2));
end.
