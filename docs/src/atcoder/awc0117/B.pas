program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 200 * 1000 + 1;
	INF = int64(1) shl 60;
var
	n, q, i, j, k: int32;
	tp: int8;
	v, ans: int64;
	c: array [1 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do read(c[i]);
	readln;

	i := 1;
	ans := 0;
	for j := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				readln(v);
				inc(ans, v);

				while (i <= n) and (ans >= c[i]) do begin
					dec(ans, c[i]);
					inc(i);
				end;
			end;

			2: begin
				readln(k);
				if k < i then
					writeln(c[k])
				else if k > i then
					writeln(0)
				else
					writeln(ans);
			end;

		end;
	end;
end.
