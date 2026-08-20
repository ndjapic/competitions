program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #grid
var
	n, a, b, p, q, r, s, i, j: int64;
	x, y: int32;
	ans: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, a, b);
	readln(p, q, r, s);

	setlength(ans, s - r + 1);
	for x := 1 to q - p + 1 do begin
		i := p + x - 1;
		for y := 1 to s - r + 1 do begin
			j := r + y - 1;
			if (i - a = j - b) or (i - a = b - j) then
				ans[y] := '#'
			else
				ans[y] := '.';
		end;
		writeln(ans);
	end;
end.
