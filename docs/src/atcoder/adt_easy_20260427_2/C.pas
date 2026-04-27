program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	hh = 100;
var
	h, w, n, i, j, k, dir: int32;
	s: array [1 .. hh] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(h, w, n);

	for i := 1 to h do begin
		setlength(s[i], w);
		for j := 1 to w do s[i][j] := '.';
	end;


	i := 1;
	j := 1;
	dir := 0;
	for k := 1 to n do begin
		case s[i][j] of

			'.': begin
				s[i][j] := '#';
				dir := (dir + 1) mod 4;
			end;

			'#': begin
				s[i][j] := '.';
				dir := (dir + 3) mod 4;
			end;

		end;
		case dir of

			0: begin
				dec(i);
				if i = 0 then i := h;
			end;

			1: begin
				inc(j);
				if j > w then j := 1;
			end;

			2: begin
				inc(i);
				if i > h then i := 1;
			end;

			3: begin
				dec(j);
				if j = 0 then j := w;
			end;

		end;
	end;

	for i := 1 to h do writeln(s[i]);
end.
