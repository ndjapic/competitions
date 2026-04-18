program C_Separated_Lunch;
{$mode delphi}
uses
    math;
const
    nn = 20;
var
    n, i: int8;
    m: int32;
    t, s, mn: int64;
    k: array [0 .. nn] of int8;

begin
    readln(n);

    t := 0;
    for i := 0 to n-1 do begin
        read(k[i]);
        inc(t, k[i]);
    end;
    readln;

    mn := high(int64);
    for m := 0 to (int32(1) shl n) - 1 do begin
        s := 0;
        for i := 0 to n-1 do
            if odd(m shr i) then inc(s, k[i]);
        mn := min(mn, max(s, t-s));
    end;

    writeln(mn);
end.
