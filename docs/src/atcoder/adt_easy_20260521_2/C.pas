program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	HH = 100;
var
	h, w, i, j, x, y: int8;
	n, k, c: int32;
	s: array [1 .. HH] of string;
	t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w, x, y);
	for i := 1 to h do readln(s[i]);
	readln(t);
	n := length(t);

	c := 0;
	for k := 1 to n do begin
		i := x;
		j := y;
		case t[k] of
			'U': if (i > 1) and (s[i-1][j] <> '#') then dec(i);
			'D': if (i < h) and (s[i+1][j] <> '#') then inc(i);
			'L': if (j > 1) and (s[i][j-1] <> '#') then dec(j);
			'R': if (j < w) and (s[i][j+1] <> '#') then inc(j);
		end;

		if (i <> x) or (j <> y) then begin
			if s[i][j] = '@' then begin
				s[i][j] := '.';
				inc(c);
			end;
			x := i;
			y := j;
		end;
	end;

	writeln(x, ' ', y, ' ', c);
end.
