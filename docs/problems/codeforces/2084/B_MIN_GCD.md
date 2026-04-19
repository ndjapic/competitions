# Problem: B_MIN_GCD.pas

```pascal
program B_MIN_GCD;
{$MODE DELPHI}{$INLINE ON}
const
    nn = 100 * 1000;
var
    ntc, tci: int16;
    n, i, i0, ioi: int32;
    istr: string;
    mn, g: int64;
    a: array [1 .. nn] of int64;

function readint64(): int64; inline;
var
    ans: int64;
begin
    ans := 0;
    while (istr[ioi] < '0') or (istr[ioi] > '9') do inc(ioi);
    while (istr[ioi] >= '0') and (istr[ioi] <= '9') do begin
        ans := ans * 10 + ord(istr[ioi]) - ord('0');
        inc(ioi);
    end;
    result := ans;
end;

function gcd(a, b: int64): int64; inline;
begin
    if b = 0 then
        gcd := a
    else
        gcd := gcd(b, a mod b);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        readln(istr);
        istr := istr + ' ';
        ioi := 1;
        a[1] := readint64();
        i0 := 1;

        for i := 2 to n do begin
            a[i] := readint64();
            if a[i] < a[i0] then i0 := i;
        end;

        g := 0;
        mn := a[i0];
        for i := 1 to n do
            if (i <> i0) and (g <> mn) and (a[i] mod mn = 0) then
                g := gcd(a[i], g);

        if g = a[i0] then
            writeln('Yes')
        else
            writeln('No');

    end;
end.

```
