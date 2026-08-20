program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 3000;
var
	n, q, i, j, l, r, v: int32;
	tp: int8;
	ans: int64;
	s, c: array [1 .. NN] of int32;
	active: array [1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do read(s[i]); readln;
	for i := 1 to n do read(c[i]); readln;
	for i := 1 to n do active[i] := true;

	for j := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				readln(l, r, v);
				for i := l to r do
					if active[i] then inc(c[i], v);
			end;

			2: begin
				readln(i);
				active[i] := false;
			end;

			3: begin
				readln(l, r);
				ans := 0;
				for i := l to r do
					if active[i] and (c[i] <= 0) then inc(ans, s[i]);
				writeln(ans);
			end;

		end;
	end;
end.
