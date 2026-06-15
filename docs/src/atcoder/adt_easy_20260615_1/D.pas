program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	HH = 100;
var
	h, w, i, j, k, x, y: int8;
	snuke: string;
	found: boolean;
	s: array [1 .. HH] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	for i := 1 to h do readln(s[i]);
	snuke := 'snuke';
	found := false;

	for i := 1 to h do if not found then
		for j := 1 to w do if not found then
			for x := -1 to 1 do
				for y := -1 to 1 do
					if not found and
						(0 < i+4*x) and (i+4*x <= h) and
						(0 < j+4*y) and (j+4*y <= w)
					then begin

						found := true;
						for k := 0 to 4 do
							found := found and (s[i+k*x][j+k*y] = snuke[k+1]);

						if found then
							for k := 0 to 4 do writeln(i+k*x, ' ', j+k*y);

					end;
end.
