# Problem: B_Annoying_String_Problem.pas

```pascal
program B_Annoying_String_Problem;
{$mode objfpc}{$H+}{$J-}
var
    ntc, tci: int32;
    i, j: int32;
    s, x, y: string;
    ans: boolean;
    cx, cy: array [0 .. 1] of int32;

function gcd(a, b: int32): int32;
begin
    if b = 0 then
        result := a
    else
        result := gcd(b, a mod b);
end;

procedure solve(d0, d1: int32);
var
    n, p, i: int32;
begin
    ans := d1 > 0;
    if ans then begin

        n := length(s);
        p := d1 div gcd(y1-x1, x0-y0);
        ans := n mod p = 0;

        if ans then begin

            i := p+1;
            while (i <= n) and (s[i-p] = s[i]) do inc(i);
            ans := i > n;

        end;

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(s);
        readln(x);
        readln(y);

        cx[0] := 0;
        cx[1] := 0;
        for i := 1 to length(x) do inc( cx[ ord(x[i]) - ord('0') ] );

        cy[0] := 0;
        cy[1] := 0;
        for j := 1 to length(y) do inc( cy[ ord(y[j]) - ord('0') ] );

        if cx[0] > cy[0] then
            solve(cx[0]-cy[0], cy[1]-cx[1])
        else if cy[0] > cx[0] then
            solve(cy[0]-cx[0], cx[1]-cy[1])
        else
            ans := cx[1] = cy[1];

        if ans then
            writeln('Yes')
        else
            writeln('No');

    end;
end.

```
