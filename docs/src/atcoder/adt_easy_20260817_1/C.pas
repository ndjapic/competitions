program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	HH = 50;
var
	h, w, si, sj, i, j: int8;
	ch: char;
	c: array [1 .. HH] of string;
	x: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);
	readln(si, sj);

	for i := 1 to h do readln(c[i]);

	readln(x);
	i := si;
	j := sj;

	for ch in x do
		case ch of
			'L': if (j > 1) and (c[i][j-1] = '.') then dec(j);
			'R': if (j < w) and (c[i][j+1] = '.') then inc(j);
			'U': if (i > 1) and (c[i-1][j] = '.') then dec(i);
			'D': if (i < h) and (c[i+1][j] = '.') then inc(i);
		end;

	writeln(i, ' ', j);
end.
