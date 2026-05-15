program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 5000;
var
	n, q, i, j, a, b, v, x, l, r: int32;
	tp: int8;
	ans: int64;
	score: array [1 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do score[i] := 0;

	for j := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				readln(a, b, v);
				dec(score[a], v);
				inc(score[b], v);
			end;

			2: begin
				readln(x, l, r);
				ans := 0;
				for i := l to r do
					if score[i] > score[x] then inc(ans);
				writeln(ans);
			end;

			3: begin
				readln(l, r, v);
				for i := l to r do
					inc(score[i], v);
			end;

		end;
	end;
end.
