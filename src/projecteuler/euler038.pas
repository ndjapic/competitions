program Pandigital_multiples;
uses
    math;
var
    n, m: int32;
    i, j, k, d: int8;
    c: array [0 .. 9] of int8;

function mnc(): int8;
var
    ans, j: int8;
begin
    ans := 1;
    for j := 1 to k do ans := min(ans, c[j]);
    mnc := ans;
end;

function mxc(): int8;
var
    ans, j: int8;
begin
    ans := 0;
    for j := 1 to k do ans := max(ans, c[j]);
    mxc := ans;
end;

function add(x: int32): int8;
var
    d: int8;
begin
    d := 1;
    while (x > 0) and (d > 0) and (d <= k) do begin
        d := x mod 10;
        if (d > 0) and (d <= k) then
            inc(c[x mod 10]);
        x := x div 10;
    end;
    add := d;
end;

begin
    readln(n, k);
    for m := 2 to n do begin

        for j := 1 to k do c[j] := 0;

        i := 1;
        d := 1;
        while (mnc() = 0) and (mxc() <= 1) and (d > 0) and (d <= k) do begin
            d := add(m*i);
            inc(i);
        end;

        if (d > 0) and (d <= k) and (mxc() <= 1) then writeln(m);

    end;
end.
