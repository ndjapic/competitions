# Problem: A_Neq_Number.pas

```pascal
program A_Neq_Number;
const
    maxe = 18;
    inf = int64(1) shl 60;
var
    ntc, tci, e: int8;
    k, x, l, r: int64;
    pow9: array [0 .. maxe] of int64;
    d: array [0 .. 18] of int8;

function neqord(x: int64): int64;
var
    ans: int64;
    e: int8;
begin
    ans := 0;
    e := 0;
    while x > 0 do begin
        d[e] := x mod 10;
        x := x div 10;
        inc(ans, pow9[e]);
        inc(e);
    end;

    d[e] := 0;
    dec(e);

    while (e >= 0) and (d[e] <> d[e+1]) do begin
        inc(ans, pow9[e] * d[e]);
        if d[e] > d[e+1] then dec(ans, pow9[e]);
        dec(e);
    end;

    if e >= 0 then inc(ans, pow9[e] * (d[e] {- 1}));

    neqord := ans;
end;

begin
    pow9[0] := 1;
    for e := 1 to maxe do pow9[e] := pow9[e-1] * 9;

    readln(ntc);
    for tci := 1 to ntc do begin
        readln(k);
        l := 1;
        r := inf;
        while r-l > 1 do begin
            x := (l+r) div 2;
            if neqord(x) > k then
                r := x
            else
                l := x;
        end;
        writeln(l);
    end;
end.

```
