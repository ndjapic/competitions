program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	hh = 50;
var
	h, w, i, j, k: int8;
	x: string;
	c: array [1 .. hh] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);
	readln(i, j);

	for i := 1 to h do readln(c[i]);

	readln(x);

	for k := 1 to length(x) do
		case x[k] of
			'L': if (j > 1) and (c[i][j-1] = '.') then dec(j);
			'R': if (j < w) and (c[i][j+1] = '.') then inc(j);
			'U': if (i > 1) and (c[i-1][j] = '.') then dec(i);
			'D': if (i < h) and (c[i+1][j] = '.') then inc(i);
		end;

	writeln(i, ' ', j);
end.
