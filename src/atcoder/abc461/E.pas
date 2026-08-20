program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unsolved
const
	NN = 300 * 1000;
var
	n, q, i, x: int32;
	tp: int8;
	ans: int64;
	cb, cw: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for x := 1 to n do begin
		cb[x] := 0;
		cw[x] := n;
	end;

	ans := 0;
	for i := 1 to q do begin
		readln(tp, x);
		case tp of

			1: begin
				inc(ans, n - cb[x]);
				cb[x] := n;
			end;

			2: begin
				inc(ans, n - cw[x]);
				cw[x] := n;
			end;

		end;
		writeln(ans);
	end;
end.
