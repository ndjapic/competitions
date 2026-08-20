program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 99;
var
	n, m, i, j, d, i0: int8;
	s: array [1 .. nn] of string;
	score: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do begin
		readln(s[i]);
		score[i] := 0;
	end;

	for j := 1 to m do begin

		d := 0;
		for i := 1 to n do
			case s[i][j] of
				'0': dec(d);
				'1': inc(d);
			end;

		if (-n < d) and (d < n) then
			if d > 0 then begin
				for i := 1 to n do
					if s[i][j] = '0' then inc(score[i])
			end else begin
				for i := 1 to n do
					if s[i][j] = '1' then inc(score[i]);
			end;

	end;

	i0 := 1;
	for i := 2 to n do
		if score[i0] < score[i] then i0 := i;

	write(i0);
	for i := i0+1 to n do
		if score[i] = score[i0] then write(' ', i);
	writeln;
end.
