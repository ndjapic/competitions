program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	hh = 10;
type
	tcell = record
		i, j: int8;
	end;
var
	h, w, i, d, x, ans: int8;
	c: tcell;
	s: array [1 .. hh] of string;
	hum: array [1 .. 2] of tcell;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function dist(c1, c2: tcell): int8;
begin
	dist := abs(c1.i - c2.i) + abs(c1.j - c2.j);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w, d);

	for i := 1 to h do readln(s[i]);

	ans := 0;
	hum[1].i := 1;
	while hum[1].i <= h do begin
		hum[1].j := 1;
		while hum[1].j <= w do begin
			if s[hum[1].i][hum[1].j] = '.' then begin

				hum[2].i := 1;
				while hum[2].i <= h do begin
					hum[2].j := 1;
					while hum[2].j <= w do begin
						if s[hum[2].i][hum[2].j] = '.' then begin

							x := 0;
							c.i := 1;
							while c.i <= h do begin
								c.j := 1;
								while c.j <= w do begin
									if s[c.i][c.j] = '.' then begin

										if min(dist(c, hum[1]), dist(c, hum[2])) <= d then inc(x);

									end;
									inc(c.j);
								end;
								inc(c.i);
							end;
							ans := max(ans, x);

						end;
						inc(hum[2].j);
					end;
					inc(hum[2].i);
				end;

			end;
			inc(hum[1].j);
		end;
		inc(hum[1].i);
	end;

	writeln(ans);
end.
