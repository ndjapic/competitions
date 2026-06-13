program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 10;
var
	h, w, d, i, j, i1, j1, i2, j2, c, ans: int8;
	s: array [1 .. NN] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function md(i1, j1, i2, j2: int8): int8;
begin
	result := abs(i1 - i2) + abs(j1 - j2);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w, d);

	for i := 1 to h do readln(s[i]);

	ans := 0;
	for i1 := 1 to h do
		for j1 := 1 to w do
			if s[i1][j1] = '.' then
				for i2 := 1 to h do
					for j2 := 1 to w do
						if s[i2][j2] = '.' then begin
							c := 0;
							for i := 1 to h do
								for j := 1 to w do
									if (s[i][j] = '.') and (
										(md(i, j, i1, j1) <= d) or
										(md(i, j, i2, j2) <= d)
									) then inc(c);
							ans := max(ans, c);
						end;

	writeln(ans);
end.
