program G1_Ruler;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
	nn = 200 * 1000;
var
    ntc, tci: int16;
    l, r, m, a: int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		l := 1;
		r := 1000;
		while r-l > 1 do begin
			m := (l+r) div 2;
			writeln('? 1 ', m); flush(output);
			readln(a);
			if a = m then
				l := m
			else
				r := m;
		end;

		writeln('! ', r);

    end;
end.
