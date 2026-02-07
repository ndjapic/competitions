program A_Homework;
{$MODE DELPHI}
uses
	math;
const
	nn = 500;
var
	ntc, tci: int16;
	n, m, i, v, d: int8;
	a, b, c: string;
	s: array [1 .. 30] of char;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);
		readln(a);
		readln(m);
		readln(b);
		readln(c);

		for i := 1 to n do begin
			s[10+i] := a[i];
		end;

		v := 11;
		d := 10+n;

		for i := 1 to m do
			case c[i] of

				'V': begin
					dec(v);
					s[v] := b[i];
				end;

				'D': begin
					inc(d);
					s[d] := b[i];
				end;

			end;

		for i := v to d do write(s[i]); writeln;

	end;
end.
