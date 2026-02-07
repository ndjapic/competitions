program C_Snake_Numbers;
{$mode delphi}
const
    maxe = 18;
var
    l, r: int64;
    d, e: int8;
    pw: array [0 .. 9, 0 .. maxe] of int64;
    spw: array [0 .. 9, 0 .. maxe] of int64;

function f(x: int64): int64;
var
    e, msd, i: int8;
    flag: boolean;
    digits: array [0 .. 18] of int8;
begin
    e := 0;
    while x > 0 do begin
        digits[e] := x mod 10;
        x := x div 10;
        inc(e);
    end;

    dec(e);
    msd := digits[e];
    result := 0;

    i := e-1;
    flag := false;
    while i >= 0 do begin
        if not flag then flag := digits[i] >= msd;
        if flag then digits[i] := msd-1;
        result := result * msd + digits[i];
        dec(i);
    end;

    inc(result, 1 + spw[msd-1, e]);
    for i := 0 to e-1 do inc(result, spw[9, i]);
end;

begin
    for d := 0 to 9 do begin
        pw[d, 0] := 1;
        spw[d, 0] := d;
    end;

    for e := 1 to maxe do begin
        spw[0, e] := 0;
        for d := 1 to 9 do begin
            pw[d, e] := pw[d, e-1] * d;
            spw[d, e] := spw[d-1, e] + pw[d, e];
        end;
    end;

    readln(l, r);
    writeln( f(r) - f(l-1) );
end.
