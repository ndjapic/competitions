program E_Triple_Operations;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
	nn = 200 * 1000;
var
    ntc, tci: int16;
    n, l, r: int32;
    a: array [0 .. nn] of int32;

begin
	a[0] := 0;
	for n := 1 to nn do a[n] := a[n div 3] + 1;
	for n := 1 to nn do inc(a[n], a[n-1]);

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(l, r);
		writeln(a[r] - a[l-1] + a[l] - a[l-1]);

    end;
end.
