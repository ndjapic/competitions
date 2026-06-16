program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #naive
const
	NN = 50 * 1000;
	CC = 50;
var
	n, q, i, j, l, r, ans: int32;
	c, h, tp: int8;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, c, q);

	for i := 1 to n do read(a[i]);
	readln;

	for j := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				readln(i, h);
				a[i] := h;
			end;

			2: begin
				readln(l, r, h);
				ans := 0;
				for i := l to r do
					if h >= a[i] then begin
						inc(ans);
						dec(h, a[i]);
					end;
				writeln(ans);
			end;

		end;
	end;
end.
