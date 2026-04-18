# Задатак: G2_Ruler.pas

```pascal
program G2_Ruler;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
	nn = 200 * 1000;
var
    ntc, tci: int16;
    l, r, a, b, area: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		l := 1;
		r := 1000;
		while r-l > 1 do begin

			a := (2*l+r+1) div 3;
			b := (l+2*r+1) div 3;

			writeln('? ', a, ' ', b); flush(output);
			readln(area);

			if area = a*b then
				l := b
			else if area = (a+1) * (b+1) then
				r := a
			else begin
				l := a;
				r := b;
			end;

		end;

		writeln('! ', r);

    end;
end.

```
